require 'timeout'
require 'tmpdir'

Around do |scenario, block|
  tmpdir = Dir.mktmpdir('cuke_expenses')

  `stack install --local-bin-path #{tmpdir}`
  cwd = Dir.pwd
  Dir.chdir(tmpdir)

  block.call

  Dir.chdir(cwd)
end

After do
  # Process.kill("-KILL", @proc.pid)
  begin
    Timeout.timeout(2) do
      Process.wait(@proc.pid)
    end
  rescue Timeout::Error
    Process.kill('-TERM', @proc.pid)
  end
end

Given("an expenses file {string}") do |filename, file_content|
  IO.write(filename, file_content)
end

Given("a ledger file {string}") do |filename, file_content|
  IO.write(filename, file_content)
end

When("I run the command {string}") do |command|
  @proc = IO.popen("./#{command}", "r+")
end

Then("the standard output should be") do |expected_output|
  Timeout.timeout(1) do
    @proc.close_write
    actual_outp = @proc.read
    expect(actual_outp).to eql expected_output
  end
end

Then("the file {string} should have content") do |outputf, expected_content|
  # Wait for file to exist first. In scenarios where STDIN is used,
  # the step sometimes executes before the file is ready.
  attempt = 0
  loop do
    break if File.file?(outputf) || attempt > 5
    sleep 0.5
    attempt += 1
  end

  content = File.read(outputf)
  expect(content).to eql expected_content
end

Then("input {string}") do |input_s|
  @proc.puts input_s
end
