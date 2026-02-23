## ----results="hide", message=FALSE--------------------------------------------
# Import libraries required for the vignette
library(eyelinker)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)

## ----results="hide", message=FALSE--------------------------------------------
# Read in example data
fpath <- system.file("extdata/mono250.asc.gz", package = "eyelinker")
asc <- read_asc(fpath)

## -----------------------------------------------------------------------------
asc$msg

## -----------------------------------------------------------------------------
trial_phases <- asc$msg %>%
  mutate(
    phase = case_when(
      str_detect(text, "Initial_display") ~ "onset",
      str_detect(text, "Target_display") ~ "target_on",
      str_detect(text, "End_trial") ~ "trial_end"
    )
  ) %>%
  subset(!is.na(phase)) %>%
  select(-c(text)) %>%
  mutate(
    phase = as.factor(phase)
  )

## -----------------------------------------------------------------------------
trial_phases

## -----------------------------------------------------------------------------
sync_timestamps <- function(df, raw) {

  # Get tracker sample rate and block onset timestamps from raw pupil data
  ms_per_sample <- median(lead(raw$time)[1:10] - raw$time[1:10])
  block_onsets <- raw %>%
    group_by(block) %>%
    summarize(onset = time[1])

  # Adjust df so that its timestamps map onto the nearest sample timestamp
  df <- df %>%
    left_join(block_onsets, by = "block") %>%
    group_by(block) %>%
    mutate(
      time = round((time - onset) / ms_per_sample) * ms_per_sample + onset
    ) %>%
    select(-c(onset))

  df
}

trial_phases <- sync_timestamps(trial_phases, asc$raw)

## -----------------------------------------------------------------------------
rawdat <- asc$raw %>%
  left_join(trial_phases, by = c("block", "time")) %>%
  group_by(block) %>%
  fill(phase, .direction = "down")

## -----------------------------------------------------------------------------
rawdat <- rawdat %>%
  subset(!is.na(phase)) %>%
  group_by(block) %>%
  mutate(
    trialtime = time - time[1]
  )

## -----------------------------------------------------------------------------
rawdat

## ----fig.width=6, fig.height=4------------------------------------------------
# Plot pupil size across the first 4 trials, color-coding by phase
ggplot(subset(rawdat, block < 5), aes(x = trialtime, y = ps, color = phase)) +
  geom_line() +
  facet_wrap(~ block) +
  labs(color = "Phase") +
  ylab("Pupil Size") +
  xlab("Trial Time (ms)")

## -----------------------------------------------------------------------------
get_epochs <- function(msg, start, end = NULL, pad = c(0, 0)) {
  end <- ifelse(is.null(end), start, end)
  epochs <- msg %>%
    group_by(block) %>%
    # Select start/end event messages for each trial
    filter(grepl(start, text) | grepl(end, text)) %>%
    # If start != end and trial has no end event, discard trial
    filter(n() >= ifelse(start == end, 1, 2)) %>%
    # Get start and end timestamps for each trial, adding padding
    summarize(
      start = time[grepl(start, text)] - pad[1],
      end = time[grepl(end, text)] + pad[2],
    )
  epochs
}

within_epoch <- function(time, epochs) {
  time %within% cbind(epochs$start, epochs$end)
}

## -----------------------------------------------------------------------------
# Get the window of data between the trial start and end events for each trial
trial_windows <- get_epochs(asc$msg, start = "Initial", end = "End")

# Get the window of data 100 ms pre-target to 200 ms post-target for each trial
target_windows <- get_epochs(asc$msg, start = "Target", pad = c(100, 200))

## -----------------------------------------------------------------------------
raw_trial <- asc$raw %>%
  filter(within_epoch(time, trial_windows))

raw_target <- asc$raw %>%
  filter(within_epoch(time, target_windows))

## -----------------------------------------------------------------------------
raw_target %>%
  group_by(block) %>%
  summarize(duration = max(time) - min(time))

## -----------------------------------------------------------------------------
post_target <- get_epochs(asc$msg, start = "Target", end = "End")

asc$sacc %>%
  filter(within_epoch(stime, post_target))

