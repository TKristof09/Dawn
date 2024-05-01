#include <filesystem>
#include <fstream>
#include <string>
#include <stdexcept>
#include <cstdlib>
#include <print>
#include <sys/wait.h>

struct CommandOutput
{
    // TODO: add args and stding when we implement support for them in the compiler
    std::string stdout;
    std::string stderr;
    int returnCode;
};

CommandOutput Merge(const CommandOutput& lhs, const CommandOutput& rhs)
{
    CommandOutput result;
    result.stdout     = lhs.stdout + rhs.stdout;
    result.stderr     = lhs.stderr + rhs.stderr;
    result.returnCode = rhs.returnCode;
    return result;
}

struct TestCase
{
    std::string expectedStdout;
    std::string expectedStderr;
    int expectedReturnCode;
};

CommandOutput ExecuteCommand(const std::string& command)
{
    CommandOutput result;
    std::println("[RUNNING] {}", command);

    // Create pipes for stdout and stderr
    int stdoutPipe[2];
    int stderrPipe[2];

    if(pipe(stdoutPipe) == -1 || pipe(stderrPipe) == -1)
    {
        throw std::runtime_error("pipe() failed!");
    }

    pid_t pid = fork();
    if(pid == -1)
    {
        throw std::runtime_error("fork() failed!");
    }
    else if(pid == 0)
    {  // Child process
        // Close unused ends of pipes
        close(stdoutPipe[0]);
        close(stderrPipe[0]);

        // Redirect stdout and stderr to pipes
        dup2(stdoutPipe[1], STDOUT_FILENO);
        dup2(stderrPipe[1], STDERR_FILENO);

        // Close original pipe ends
        close(stdoutPipe[1]);
        close(stderrPipe[1]);

        // Execute the command
        execlp("/bin/sh", "/bin/sh", "-c", command.c_str(), NULL);

        // If execlp returns, it indicates failure
        exit(EXIT_FAILURE);
    }
    else
    {
        // Parent process
        // Close unused ends of pipes
        close(stdoutPipe[1]);
        close(stderrPipe[1]);

        // Read from stdout pipe
        char buffer[128];
        ssize_t bytesRead;
        while((bytesRead = read(stdoutPipe[0], buffer, sizeof(buffer))) > 0)
        {
            result.stdout.append(buffer, bytesRead);
        }

        // Read from stderr pipe
        while((bytesRead = read(stderrPipe[0], buffer, sizeof(buffer))) > 0)
        {
            result.stderr.append(buffer, bytesRead);
        }

        // Close pipe ends
        close(stdoutPipe[0]);
        close(stderrPipe[0]);

        // Wait for the child process to finish
        int status;
        waitpid(pid, &status, 0);

        // Get the return code
        result.returnCode = WIFEXITED(status) ? WEXITSTATUS(status) : -1;
    }

    return result;
}

CommandOutput CompileAndRun(std::filesystem::path& path)
{
    std::string command          = "build/Dawn " + path.string();
    CommandOutput compilerOutput = ExecuteCommand(command);

    if(compilerOutput.returnCode != 0)
    {
        return compilerOutput;
    }

    std::string asmFile           = path.replace_extension(".asm").string();
    command                       = "fasm " + asmFile + " > /dev/null";  // suppress the fasm splash output, asm errors go to stderr
    CommandOutput assemblerOutput = ExecuteCommand(command);

    if(assemblerOutput.returnCode != 0)
    {
        return Merge(compilerOutput, assemblerOutput);
    }

    std::string executable  = path.replace_extension().string();
    command                 = executable;
    CommandOutput runOutput = ExecuteCommand(command);

    return Merge(Merge(compilerOutput, assemblerOutput), runOutput);
}

TestCase ParseTestFile(const std::filesystem::path& path)
{
    TestCase testCase;


    std::ifstream file(path.string(), std::ios::in);
    if(!file.is_open())
    {
        std::println("[ERROR] Could not open file {}", path.string());
        exit(1);
    }

    std::string line;
    enum Section
    {
        STDOUT,
        STDERR,
        RETURN_CODE,
        NONE
    };
    Section currentSection = NONE;

    while(std::getline(file, line))
    {
        if(line.substr(0, 7) == "stdout:")
        {
            currentSection          = STDOUT;
            testCase.expectedStdout = "";
        }
        else if(line.substr(0, 7) == "stderr:")
        {
            currentSection          = STDERR;
            testCase.expectedStderr = "";
        }
        else if(line.substr(0, 13) == "return_code:")
        {
            currentSection = RETURN_CODE;
        }
        else if(currentSection == STDOUT)
        {
            testCase.expectedStdout += line + "\n";
        }
        else if(currentSection == STDERR)
        {
            testCase.expectedStderr += line + "\n";
        }
        else if(currentSection == RETURN_CODE)
        {
            testCase.expectedReturnCode = std::stoi(line);
        }
    }

    return testCase;
}

bool CheckTest(const std::filesystem::path& entry, TestCase& testCase, CommandOutput& result)
{
    bool success = true;

    if(testCase.expectedStdout != result.stdout)
    {
        std::println("[FAIL] {}: stdout does not match expected output", entry.string());
        std::println("Expected:\n{}", testCase.expectedStdout);
        std::println("Got:\n{}", result.stdout);
        success = false;
    }

    if(testCase.expectedStderr != result.stderr)
    {
        std::println("[FAIL] {}: stderr does not match expected output", entry.string());
        std::println("Expected:\n{}", testCase.expectedStderr);
        std::println("Got:\n{}", result.stderr);
        success = false;
    }

    if(testCase.expectedReturnCode != result.returnCode)
    {
        std::println("[FAIL] {}: return code does not match expected output", entry.string());
        std::println("Expected:\n{}", testCase.expectedReturnCode);
        std::println("Got:\n{}", result.returnCode);
        success = false;
    }

    return success;
}

void RecordTestOutput(const std::filesystem::path& entry, CommandOutput& result)
{
    std::ofstream file(entry.string(), std::ios::out);
    if(!file.is_open())
    {
        std::println("[ERROR] Could not open file {}", entry.string());
        exit(1);
    }

    file << "stdout:\n"
         << result.stdout;
    if(!result.stdout.empty() && result.stdout.back() != '\n')
    {
        file << "\n";
    }
    file << "stderr:\n"
         << result.stderr;
    if(!result.stderr.empty() && result.stderr.back() != '\n')
    {
        file << "\n";
    }
    file << "return_code:\n"
         << result.returnCode << "\n";
}

void RunTests()
{
    // iterate files in test directory
    bool success = true;
    for(const auto& entry : std::filesystem::directory_iterator("test"))
    {
        if(entry.path().extension() != ".eosTest")
        {
            continue;
        }

        // check if corresponding .eos file exists
        auto path    = entry.path();
        auto eosFile = path.replace_extension(".eos");
        if(!std::filesystem::exists(eosFile))
        {
            std::println("[ERROR] Corresponding .eos file not found for {}", entry.path().string());
            continue;
        }


        std::string testFile = entry.path().string();


        std::println("[TESTING] {}", eosFile.string());

        CommandOutput result = CompileAndRun(eosFile);

        TestCase testCase = ParseTestFile(testFile);
        bool testResult   = CheckTest(entry.path(), testCase, result);
        if(testResult)
        {
            std::println("[PASSED] {}", eosFile.string());
        }
        else
        {
            std::println("[FAILED] {}", eosFile.string());
        }
        success = testResult && success;
    }

    if(!success)
    {
        exit(1);
    }
}

void UpdateTestOutputs()
{
    // iterate files in test directory
    for(const auto& entry : std::filesystem::directory_iterator("test"))
    {
        if(entry.path().extension() != ".eos")
        {
            continue;
        }


        auto eosFile = entry.path();


        std::println("[UPDATING] {}", eosFile.string());

        CommandOutput result = CompileAndRun(eosFile);

        RecordTestOutput(eosFile.replace_extension(".eosTest"), result);
    }
}

int main(int argc, char** argv)
{
    if(argc < 2)
    {
        std::println("Usage: {} <command>", argv[0]);
        std::println("Commands:");
        std::println("  run: Run tests");
        std::println("  update: Update test outputs");
        return 1;
    }

    std::string command = argv[1];
    if(command == "run")
    {
        RunTests();
    }
    else if(command == "update")
    {
        UpdateTestOutputs();
    }
    else
    {
        std::println("Unknown command: {}", command);
        return 1;
    }

    return 0;
}
