# Check if at least the program name is provided
if ($args.Count -lt 1) {
    Write-Host "Usage: ./script.ps1 <program_name> [arguments...]"
    exit
}

# Get the program name (first argument)
$program = $args[0]

# Get the arguments (all arguments after the program name)
$programArgs = $args[1..$($args.Count - 1)] -join ' '

# Measure the time taken and capture the output
$time = Measure-Command {
    # Execute the program with the arguments
    $output = $(& "./$program" $programArgs | Out-String)
    # Print the output
    Write-Host $output
}

# Print the elapsed time
Write-Host "Elapsed time: $($time.TotalSeconds) seconds"

