library(markmyassignment)
assignment_path <- 
  file.path(system.file(package = "markmyassignment"), "extdata", "example_assignment01.yml")
set_assignment(assignment_path)

show_tasks()
task1 <- c(pi, exp(1))
print(task1)

mark_my_assignment()
my_name <- 'ian'

task2 <- function(vector){
  vector[1] + vector[length(vector)]
}
task2(1:5)
