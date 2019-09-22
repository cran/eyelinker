## ----results="hide", message=FALSE---------------------------------------
# Import libraries required for the vignette
require(eyelinker)
require(dplyr)
require(tidyr)
require(ggplot2)
require(intervals)
require(stringr)

## ----results="hide", message=FALSE---------------------------------------
# Get path of example file in package
fpath <- system.file("extdata/mono500.asc.gz", package = "eyelinker")

## ------------------------------------------------------------------------
dat <- read.asc(fpath)
str(dat, max.level = 1)

## ------------------------------------------------------------------------
dat_noraw <- read.asc(fpath, samples = FALSE)
str(dat_noraw, max.level = 1) # 'raw' data frame no longer present in the returned list

## ------------------------------------------------------------------------
dat_parseall <- read.asc(fpath, parse_all = TRUE)
str(dat_parseall, max.level = 1)

## ------------------------------------------------------------------------
msgs <- dat_parseall$msg
in_block <- msgs$block %% 1 == 0

# Make all out-of-block messages belong to the next block (e.g. 0.5 to 1)
msgs[!in_block, ]$block <- msgs[!in_block, ]$block + 0.5

# Drop all out-of-block messages from the data
msgs <- msgs[!in_block, ]

## ------------------------------------------------------------------------
dplyr::select(dat$info, c(model, mount, sample.rate, cr))

## ------------------------------------------------------------------------
dplyr::select(dat$info, c(left, right, mono, screen.x, screen.y))

## ------------------------------------------------------------------------
dplyr::select(dat$info, c(sample.dtype, event.dtype, pupil.dtype))

## ------------------------------------------------------------------------
raw <- dat$raw
head(raw, 3)

## ------------------------------------------------------------------------
dat.bi <- read.asc(system.file("extdata/bino1000.asc.gz", package = "eyelinker"))

head(dat.bi$raw, 3)

## ------------------------------------------------------------------------
raw.long <- dplyr::select(raw, time, xp, yp, block) %>% tidyr::gather("coord", "pos", xp, yp)
head(raw.long, 2)
tail(raw.long, 2)

## ----fig.width=6.5, fig.height=4-----------------------------------------
raw.long <- mutate(raw.long, ts = (time - min(time)) / 1e3) # let's have time in sec.
ggplot(raw.long, aes(ts, pos, col = coord)) + geom_point()

## ----fig.width=6.5, fig.height=4-----------------------------------------
ggplot(raw.long, aes(ts, pos, col = coord)) + geom_line() + facet_wrap(~block)

## ------------------------------------------------------------------------
# Downsample raw data from 500 Hz to 100 Hz by only keeping every 5th row
raw_100Hz <- dplyr::filter(raw, row_number() %% 5 == 0)
head(raw_100Hz, 4)

## ------------------------------------------------------------------------
sac <- dat$sac
head(sac, 3)

## ------------------------------------------------------------------------
Sac <- cbind(sac$stime, sac$etime) # Define a set of intervals with these endpoints
# See also: intervals package
raw <- mutate(raw, saccade = time %In% Sac)
head(raw, 3)
mean(raw$saccade) * 100 # 6% of time samples correspond to saccades

## ----fig.width=6.5, fig.height=4-----------------------------------------
mutate(raw.long, saccade = time %In% Sac) %>%
  filter(block == 1) %>%
  ggplot(aes(ts, pos, group = coord, col = saccade)) + geom_line()

## ------------------------------------------------------------------------
fix <- dat$fix
head(fix, 3)

## ----fig.width=6.5, fig.height=4-----------------------------------------
Fix <- cbind(fix$stime, fix$etime) # Define a set of intervals
mutate(raw.long, fixation = time %In% Fix) %>%
  filter(block == 1) %>%
  ggplot(aes(ts, pos, group = coord, col = fixation)) + geom_line()

## ------------------------------------------------------------------------
head(mutate(raw, fix.index = whichInterval(time, Fix)), 4)

## ------------------------------------------------------------------------
raw <- mutate(raw, fix.index = whichInterval(time, Fix))
fix.check <- filter(raw, !is.na(fix.index)) %>%
  group_by(fix.index) %>%
  summarise(axp = mean(xp), ayp = mean(yp)) %>%
  ungroup
head(fix.check, 3)

## ------------------------------------------------------------------------
all.equal(fix.check$axp, fix$axp)
all.equal(fix.check$ayp, fix$ayp)

## ------------------------------------------------------------------------
fpath <- system.file("extdata/monoRemote500.asc.gz", package = "eyelinker")
dat <- read.asc(fpath)
dat$blinks

## ------------------------------------------------------------------------
Blk <- cbind(dat$blinks$stime, dat$blinks$etime) # Define a set of intervals

head(filter(dat$raw, time %In% Blk))

## ------------------------------------------------------------------------
Suspect <- Intervals(Blk) %>% expand(100, "absolute")
Suspect

## ----fig.width=6.5, fig.height=4-----------------------------------------
raw.long <- dplyr::select(dat$raw, time, xp, yp, block) %>%
  gather("coord", "pos", xp, yp)
raw.long <- mutate(raw.long, ts = (time - min(time)) / 1e3) # let's have time in sec.
ex <- mutate(raw.long, suspect = time %In% Suspect) %>%
  filter(block == 2)

ggplot(ex, aes(ts, pos, group = coord, col = suspect)) +
  geom_line() +
  coord_cartesian(xlim = c(34, 40)) +
  labs(x = "Time (s)")

## ------------------------------------------------------------------------
head(dat$msg)

## ------------------------------------------------------------------------
filter(dat$msg, str_detect(text, "blank_screen"))

## ----fig.width=6, fig.height=4-------------------------------------------
# Get fixations/saccades for just first block
fix_b1 <- subset(dat$fix, block == 1)
sacc_b1 <- subset(dat$sacc, block == 1)

# Actually plot fixations and saccades
ggplot() +
  geom_segment(data = sacc_b1,
    aes(x = sxp, y = syp, xend = exp, yend = eyp),
    arrow = arrow(), size = 1, alpha = 0.5, color = "grey40"
  ) +
  geom_point(data = fix_b1,
      aes(x = axp, y = ayp, size = dur, color = eye),
      alpha = 0.5, color = "blue"
  ) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, dat$info$screen.x)) +
  scale_y_reverse(expand = c(0, 0), limits = c(dat$info$screen.y, 0)) +
  labs(x = "x-axis (pixels)", y = "y-axis (pixels)") +
  coord_fixed() # Keeps aspect ratio from getting distorted

## ----fig.width=6, fig.height=4-------------------------------------------
raw_b1 <- subset(dat$raw, block == 1)

ggplot() +
  geom_path(data = raw_b1, aes(x = xp, y = yp), size = 0.5, color = "firebrick2") +
  scale_x_continuous(expand = c(0, 0), limits = c(0, dat$info$screen.x)) +
  scale_y_reverse(expand = c(0, 0), limits = c(dat$info$screen.y, 0)) +
  labs(x = "x-axis (pixels)", y = "y-axis (pixels)") +
  coord_fixed() # Keeps aspect ratio from getting distorted

