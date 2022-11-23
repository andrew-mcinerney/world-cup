library(taskscheduleR)

taskscheduler_create(taskname = "test_run", rscript = "C:\\Users\\andrew.mcinerney\\Documents\\world-cup\\push_to_github.R",
                     schedule = "MINUTE", starttime = format(Sys.time() + 10, "%H:%M"), modifier = 5)
