class Student

    attr_accessor :id
    attr_accessor :name
    attr_accessor :addr
    attr_accessor :grade

    def initialize(id, name, addr, grade)
        @id = id
        @name = name
        @addr = addr
        @grade = grade
        #@@my_gvar = 33
    end
end

student1 = Student.new("V00855767", "Lee A. Deweert", "755 Caledonia Ave", "98")
student2 = Student.new("Whse09471", "Mitzy", "904 Fir St", "23")

puts student1.name
puts student1.id
puts student1.addr
puts student1.grade

student1.name = "Bob"
puts student1.name
