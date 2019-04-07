require 'tmpdir'

Given("an expenses file {string}") do |filename, file_content|
  @tmpdir = Dir.mktmpdir('cuke_expenses')
  @input_filename = File.join(@tmpdir, filename)
  puts "writing to #{@input_filename}"
  IO.write(@input_filename, file_content)
end

When("I run the command {string} with {string} and {string}") do |command, inputf, outputf|
  input_path = File.join(@tmpdir, inputf)
  output_path = File.join(@tmpdir, outputf)
  @output = `stack exec -- #{command} #{input_path} #{output_path}`
end

Then("the standard output should be") do |expected_output|
  expect(@output).to eql expected_output
end

Then("the file {string} should have content") do |outputf, expected_content|
  output_path = File.join(@tmpdir, outputf)
  content = File.read(output_path)
  expect(content).to eql expected_content
end
