## ----results="hide", message=FALSE, warning=FALSE-----------------------------
# Import libraries required for the vignette
require(eyelinker)
require(dplyr)
require(tidyr)
require(ggplot2)
require(intervals)
require(stringr)

## ----results="hide", message=FALSE--------------------------------------------
# Get path of example file in package
fpath <- system.file("extdata/mono500.asc.gz", package = "eyelinker")

## -----------------------------------------------------------------------------
dat <- read_asc(fpath)
str(dat, max.level = 1, give.attr = FALSE)

## -----------------------------------------------------------------------------
dat_noraw <- read_asc(fpath, samples = FALSE)
str(dat_noraw, max.level = 1) # 'raw' data frame no longer present in the returned list

## -----------------------------------------------------------------------------
dat_parseall <- read_asc(fpath, parse_all = TRUE)
str(dat_parseall, max.level = 1, give.attr = FALSE)

## -----------------------------------------------------------------------------
msgs <- dat_parseall$msg
in_block <- msgs$block %% 1 == 0

# Make all out-of-block messages belong to the next block (e.g. 0.5 to 1)
msgs[!in_block, ]$block <- msgs[!in_block, ]$block + 0.5

# Drop all out-of-block messages from the data
msgs <- msgs[!in_block, ]

## -----------------------------------------------------------------------------
dplyr::select(dat$info, c(model, mount, sample.rate, cr))

## -----------------------------------------------------------------------------
dplyr::select(dat$info, c(left, right, mono, screen.x, screen.y))

## -----------------------------------------------------------------------------
dplyr::select(dat$info, c(sample.dtype, event.dtype, pupil.dtype))

## -----------------------------------------------------------------------------
raw <- dat$raw
head(raw, 3)

## -----------------------------------------------------------------------------
dat_bi <- read_asc(system.file("extdata/bino1000.asc.gz", package = "eyelinker"))

head(dat_bi$raw, 3)

## -----------------------------------------------------------------------------
raw_long <- raw %>%
  dplyr::select(time, xp, yp, block) %>%
  tidyr::gather("coord", "pos", xp, yp)

head(raw_long, 2)
tail(raw_long, 2)

## ----fig.width=6.5, fig.height=4----------------------------------------------
raw_long <- raw_long %>%
  mutate(tsec = (time - min(time)) / 1e3) # let's have time in sec.

ggplot(raw_long, aes(tsec, pos, col = coord)) + geom_point()

## ----fig.width=6.5, fig.height=4----------------------------------------------
ggplot(raw_long, aes(tsec, pos, col = coord)) +
  geom_line() +
  facet_wrap(~ block)

## -----------------------------------------------------------------------------
# Downsample raw data from 500 Hz to 100 Hz by only keeping every 5th row
raw_100hz <- dplyr::filter(raw, row_number() %% 5 == 0)
head(raw_100hz, 4)

## -----------------------------------------------------------------------------
sac <- dat$sac
head(sac, 3)

## -----------------------------------------------------------------------------
sac_intervals <- cbind(sac$stime, sac$etime)
# See also: intervals package
raw <- mutate(raw, saccade = time %within% sac_intervals)
head(raw, 3)
mean(raw$saccade) * 100 # 6% of time samples correspond to saccades

## ----fig.width=6.5, fig.height=4----------------------------------------------
raw_long %>%
  mutate(saccade = time %within% sac_intervals) %>%
  filter(block == 1) %>%
  ggplot(aes(tsec, pos, group = coord, col = saccade)) + geom_line()

## -----------------------------------------------------------------------------
fix <- dat$fix
head(fix, 3)

## ----fig.width=6.5, fig.height=4----------------------------------------------
fix_intervals <- cbind(fix$stime, fix$etime)
raw_long %>%
  mutate(fixation = time %within% fix_intervals) %>%
  filter(block == 1) %>%
  ggplot(aes(tsec, pos, group = coord, col = fixation)) + geom_line()

## -----------------------------------------------------------------------------
head(mutate(raw, fix_index = which_interval(time, fix_intervals)), 4)

## -----------------------------------------------------------------------------
raw <- mutate(raw, fix_index = which_interval(time, fix_intervals))
fix_check <- raw %>%
  filter(!is.na(fix_index)) %>%
  group_by(fix_index) %>%
  summarise(axp = mean(xp), ayp = mean(yp)) %>%
  ungroup()

head(fix_check, 3)

## -----------------------------------------------------------------------------
all.equal(fix_check$axp, fix$axp)
all.equal(fix_check$ayp, fix$ayp)

## -----------------------------------------------------------------------------
fpath <- system.file("extdata/monoRemote500.asc.gz", package = "eyelinker")
dat <- read_asc(fpath)
dat$blinks

## -----------------------------------------------------------------------------
blink_intervals <- cbind(dat$blinks$stime, dat$blinks$etime)

head(filter(dat$raw, time %within% blink_intervals))

## -----------------------------------------------------------------------------
blink_regions <- Intervals(blink_intervals) %>% expand(100, "absolute")
blink_regions

## ----fig.width=6.5, fig.height=4----------------------------------------------
raw_long <- dat$raw %>%
  dplyr::select(time, xp, yp, block) %>%
  gather("coord", "pos", xp, yp) %>%
  mutate(tsec = (time - min(time)) / 1e3) # let's have time in sec.

ex <- mutate(raw_long, suspect = time %within% blink_regions) %>%
  filter(block == 2)

ggplot(ex, aes(tsec, pos, group = coord, col = suspect)) +
  geom_line() +
  coord_cartesian(xlim = c(34, 40)) +
  labs(x = "Time (s)")

## -----------------------------------------------------------------------------
head(dat$msg)

## -----------------------------------------------------------------------------
filter(dat$msg, str_detect(text, "blank_screen"))

## ----fig.width=6, fig.height=4------------------------------------------------
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

## ----fig.width=6, fig.height=4------------------------------------------------
raw_b1 <- subset(dat$raw, block == 1)

ggplot(data = raw_b1, aes(x = xp, y = yp)) +
  geom_path(size = 0.5, color = "firebrick2") +
  scale_x_continuous(expand = c(0, 0), limits = c(0, dat$info$screen.x)) +
  scale_y_reverse(expand = c(0, 0), limits = c(dat$info$screen.y, 0)) +
  labs(x = "x-axis (pixels)", y = "y-axis (pixels)") +
  coord_fixed() # Keeps aspect ratio from getting distorted

