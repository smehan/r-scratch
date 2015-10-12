# some test data
data <- rbind(c(1,1,2,3), c(1,1, 3, 4), c(1,4,6,7))
str <- "Here is a special string \n\r with \t many üñåé tokens"
# a test input set of numbers to copy to your clipboard if you have nothing to hand
# [10:17:55, 10:37:40, 10:40:26, 10:48:18, 11:00:17, 11:01:12, 11:06:58, 11:09:20, 11:43:41, 11:48:24, 11:49:14, 12:07:31, 12:10:52, 12:10:52, 12:19:00, 12:19:00, 12:19:43, 12:20:55, 12:38:27, 12:38:27, 12:55:09, 12:55:10, 12:57:31, 12:57:31, 13:04:16, 13:04:16, 13:06:51   13:06:51, 14:55:06, 14:56:10, 15:01:30, 15:28:42, 3:29:17, 15:35:33, 15:58:32, 16:05:07, 16:09:16, 16:10:36, 16:32:57, 16:32:57, 16:34:32, 16:38:16, 17:43:27, 17:53:01, 17:56:14, 18:08:21, 18:17:23, 18:37:23, 18:37:23, 18:43:13, 18:43:13   18:51:43, 18:51:43, 19:05:39, 19:05:39]

# Input works reasonably well for tables and text
cb_handle <- pipe("pbcopy", "w")                       
write.table(data, file=cb_handle)
close(cb_handle)

cb_handle <- pipe("pbcopy", "w")                       
write(str, file = cb_handle)
close(cb_handle)

# DO NOT USE THIS ONE as it leads to a runaway R process
cb_handle <- pipe("pbcopy", "r")
read.table(cb_handle)

# This reads in the contents but leaves cleanup to do if not really a table
cb_handle <- pipe("pbpaste")
data_in <- read.table(cb_handle)



