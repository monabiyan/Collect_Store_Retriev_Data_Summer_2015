# Install the package
install.packages(c("ngramr", "ggplot2"))

# Load it into R
library(ngramr)
library(ggplot2)

# Case-insensitive search
lines <- ngrami(c("Iran","Iraq"), year_start = 1913)
ggplot(lines, aes(Year, Frequency, colour = Phrase)) + theme_minimal() + geom_line(lwd = 1)
