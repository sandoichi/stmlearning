# STM Learning
Simple app for trying out STM that has threads try to randomly generate matching numbers for a set of work.


The user will be prompted for how many random numbers to generate into a work channel, and how many worker threads to launch.


The generated numbers will be in the range of `[0..1000]`, and each worker will have **50** attempts to match a number that it pulled from the work channel before being forced to move onto the next.
