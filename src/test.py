# Top-level variable definitions (var_def)
x: int = 42
name: "string" = "Hello"
flag: bool = True
numbers: [int] = None

# Function definition with typed parameters and return type
def greet(person: "string"):
    global name
    greeting: "string" = "Hi, "
    greeting = greeting + person
    print(greeting)
    return

# Recursive function definition
def factorial(n: int) -> int:
    if n == 0:
        return 1
    else:
        return n * factorial(n - 1)

# Function with a nested function using nonlocal
def outer() -> int:
    x: int = 0
    def inner():
        nonlocal x
        x = x + 1
    inner()
    inner()
    return x

# Function demonstrating control-flow statements and global usage
def control_flow_test(flag: bool):
    global x
    if flag:
        print("Flag is True")
    elif not flag:
        print("Flag is not True")
    else:
        pass

    # while loop
    while x < 50:
        x = x + 1

    # for loop with a list literal
    for i in [1, 2, 3]:
        print(i)

# Class definition with a typed variable and method
class MyClass(object):
    val: int = 10

    def method(self: "MyClass", y: int) -> int:
        return self.val + y

# -------------------------------
# Top-level statements
# -------------------------------

# Assign a list expression to a previously defined variable
numbers = [1, 2, 3]

# Multiple assignment
a = b = 10

# Arithmetic expression (including unary minus and integer division)
c = -a * 2 + 10 // 5

# Ternary expression (if ... else)
z = "yes" if a > 5 else "no"

# List indexing
numbers[0] = 100

print(z)

# Create an instance of MyClass and call its method
my_obj = MyClass()
print(my_obj.method(5))

# Simple if statement
if x < 100:
    x = x + 10

# while statement
while x < 60:
    x = x + 1

# for statement using a range
for j in range(5):
    print(j)

# Calling previously defined functions
print(outer())
print(factorial(5))
control_flow_test(flag)
greet("World")

# pass statement (simple_statement)
pass
