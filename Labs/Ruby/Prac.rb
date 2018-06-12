class MyClass
	attr_accessor :name
	def initialize (name)
		@name = name
	end
end

m = MyClass.new("Nikhil")
puts m.name