require 'fiber'

words = Fiber.new do
  STDIN.each_line do |line|
    line.scan(/\w+/) do |word|
      Fiber.yield word
    end
  end
  STDIN.close
end

counts = Hash.new(0)
while word = words.resume
  counts[word] += 1
end

counts.keys.sort.each {|k| print "#{k}: #{counts[k]}\n"}

