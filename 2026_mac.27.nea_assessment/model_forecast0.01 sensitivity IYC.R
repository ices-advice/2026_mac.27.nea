

#-------------------------------------------------------------------------------
# initialise
#-------------------------------------------------------------------------------

library(icesTAF)
taf.library(stockassessment)
library(ggplot2)
#taf.bootstrap()

mkdir("output")
mkdir("output/model diags")
mkdir("output/model diags/explorations/")




# load model run
model <- "0.01"
load(paste0("./model/fit_and_diagnostics/model_",model,"_full_output.RData"))


# Config years and reference values
ay<- max(fit$data$years)   # current year, also final data year as we have in-year data
ady <- ay + 1
Ave.yrs <- ay - (1:3)      # no biological input data for the curreny year
Rec.yrs <- 2017:ay         # take all recruitments since 2017, incl. current year
# simulation numbers
nsim <- 1000
cat("! change back to 20000 sims")
#ref points
MSYBtrigger <- 4119337
Blim        <- 3067017
Fmsy        <- 0.191
# catch estimate for 2026
IYC <- 398186
# current year advice
Cur.Adv  <- 174000




sens.n <- 1000
# assumption on proportion error on intermediate year catch estimate
p.err<-runif(sens.n,0.8,1.2)           # +-20%
# table to store results
res<-data.frame(p.err.IYC = p.err , IYC = NA, SSB2026 = NA , C2027 =NA, SSB2028 =NA,pBlim2028 = NA)



for (i in 1:sens.n)

{

# add error to IYC
eIYC <- p.err[i]*IYC


#### FC : F2027 = Fmsy
set.seed(123)
FC1<- forecast(
  fit,                                                                          #  an assessment object of type sam, as returned from the function sam.fit
  fscale   = c(NA,NA,1),                                                        #  a vector of f-scales. See details.
  catchval.exact = c(eIYC,NA,NA),                                                        #  a vector of target catches which will be met without noise. See details.
  fval     = c(NA,Fmsy,NA),                                                          #  a vector of target f values. See details.
  nosim = nsim,                                                                 #  number of simulations default is 1000
  year.base = ay,                                              #  starting year default last year in assessment. Currently it is only supported to use last assessment year or the year before
  ave.years = Ave.yrs ,                                     #  vector of years to average for weights, maturity, M and such
  rec.years = Rec.yrs ,                                     #  vector of years to use to resample recruitment from
  label = "F2027 = Fmsy",                                                                 #  optional label to appear in short table
  processNoiseF = FALSE,                                                         #  option to turn off process noise in F
  savesim = TRUE
  )

SSBs <- attr(FC1, "tab")[, "ssb:median"]




#### FC2 : F2027 = Fmsy reduced

Fred <- Fmsy * SSBs[["2026"]] / MSYBtrigger

set.seed(123)
FC2<- forecast(
  fit,                                                                          #  an assessment object of type sam, as returned from the function sam.fit
  fscale   = c(NA,NA,1),                                                        #  a vector of f-scales. See details.
  catchval.exact = c(eIYC,NA,NA),                                                        #  a vector of target catches which will be met without noise. See details.
  fval     = c(NA,Fred,NA),                                                          #  a vector of target f values. See details.
  nosim = nsim,                                                                 #  number of simulations default is 1000
  year.base = ay,                                              #  starting year default last year in assessment. Currently it is only supported to use last assessment year or the year before
  ave.years = Ave.yrs ,                                     #  vector of years to average for weights, maturity, M and such
  rec.years = Rec.yrs ,                                     #  vector of years to use to resample recruitment from
  label = "F2027 = Fred",                                                               #  optional label to appear in short table
  processNoiseF = FALSE,                                                         #  option to turn off process noise in F
  savesim = T
)


SSBs  <- attr(FC2, "tab")[, "ssb:median"]
Fbars <- attr(FC2, "tab")[, "fbar:median"]
Cs    <- attr(FC2, "tab")[, "catch:median"]
pBlim2028<-sum(FC2[[3]]$ssb < Blim)/length(FC2[[3]]$ssb)

### extract results

res$IYC[i]       <-  eIYC
res$SSB2026[i]   <-  SSBs[1]
res$SSB2028[i]   <-  SSBs[3]
res$C2027[i]     <-  Cs[2]
res$pBlim2028[i] <-  pBlim2028


}



 save(res, file= "./output/sensSTF.RData")

# merci chatGPT

library(ggplot2)
library(patchwork)


dat <- res

dat$SSB2026 <-   dat$SSB2026 / mean(dat$SSB2026)
dat$SSB2028 <-   dat$SSB2028 / mean(dat$SSB2028)
dat$C2027 <-   dat$C2027 / mean(dat$C2027)
dat$pBlim2028 <-   dat$pBlim2028 / mean(dat$pBlim2028)

# ------------------------------------------------------------
# 2. Define common axis limits
# ------------------------------------------------------------

xlim <- range(dat$SSB2026)
ylim <- range(dat$p.err.IYC)


# ------------------------------------------------------------
# 3. Main scatterplot
# ------------------------------------------------------------

p.main <- ggplot(
  dat,
  aes(x = SSB2026, y = p.err.IYC)
) +

  geom_point(
    size = 2,
    alpha = 0.6
  ) +

  # Linear regression
  geom_smooth(
    method = "lm",
    se = TRUE,
    linewidth = 0.8
  ) +

  # Keep the same ranges as the marginal densities
  coord_cartesian(
    xlim = xlim,
    ylim = ylim
  ) +

  theme_bw() +

  theme(
    panel.grid.minor = element_blank(),

    # --------------------------------------------------------
    # Remove x-axis completely
    # --------------------------------------------------------
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.line.x = element_blank(),

    # --------------------------------------------------------
    # Remove y-axis completely
    # --------------------------------------------------------
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.y = element_blank()
  )


# ------------------------------------------------------------
# 4. Density of p.err.IYC
# ------------------------------------------------------------

p.iyc <- ggplot(
  dat,
  aes(x = p.err.IYC)
) +

  geom_histogram(
    fill = "grey70",
    colour = "black",
    alpha = 0.7,
    linewidth = 0.7
  ) +

  # Rotate the density so that p.err.IYC is vertical
  coord_flip(
    xlim = ylim
  ) +

  labs(
    x = NULL,
    y = "p.err.IYC"
  ) +

  theme_bw() +

  theme(
    panel.grid.minor = element_blank(),

    # --------------------------------------------------------
    # Remove the horizontal density scale
    # --------------------------------------------------------
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank(),

    # --------------------------------------------------------
    # Keep vertical p.err.IYC axis
    # --------------------------------------------------------
    axis.text.y = element_text(size = 10),
    axis.ticks.y = element_line(),
    axis.title.y = element_text(
      size = 12,
      angle = 90
    )
  )
# ------------------------------------------------------------
# 5. Density of SSB2026
# ------------------------------------------------------------

p.ssb <- ggplot(
  dat,
  aes(x = SSB2026)
) +

  geom_histogram(
    fill = "grey70",
    colour = "black",
    alpha = 0.7,
    linewidth = 0.7
  ) +

  coord_cartesian(
    xlim = xlim
  ) +

  labs(
    x = "SSB2026",
    y = NULL
  ) +

  theme_bw() +

  theme(
    panel.grid.minor = element_blank(),

    # Remove y-axis
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.y = element_blank(),

    # Keep x-axis
    axis.title.x = element_text(size = 12),
    axis.text.x = element_text(size = 10)
  )


# ------------------------------------------------------------
# 6. Empty bottom-left panel
# ------------------------------------------------------------

p.empty <- ggplot() +
  theme_void()


# ------------------------------------------------------------
# 7. Combine plots
# ------------------------------------------------------------

figure <-
  (p.iyc | p.main) /
  (p.empty | p.ssb) +

  plot_layout(
    widths = c(0.8, 4),
    heights = c(4, 1.2)
  )


# ------------------------------------------------------------
# 8. Display figure
# ------------------------------------------------------------

figure


# ------------------------------------------------------------
# 9. Save figure
# ------------------------------------------------------------

ggsave(
  "./output/sensSTF/SSB2026_p.err.IYC_density.png",
  figure,
  width = 9,
  height = 7,
  dpi = 300
)













# ------------------------------------------------------------
# 2. Define common axis limits
# ------------------------------------------------------------

xlim <- range(dat$SSB2028)
ylim <- range(dat$p.err.IYC)


# ------------------------------------------------------------
# 3. Main scatterplot
# ------------------------------------------------------------

p.main <- ggplot(
  dat,
  aes(x = SSB2028, y = p.err.IYC)
) +

  geom_point(
    size = 2,
    alpha = 0.6
  ) +

  # Linear regression
  geom_smooth(
    method = "lm",
    se = TRUE,
    linewidth = 0.8
  ) +

  # Keep the same ranges as the marginal densities
  coord_cartesian(
    xlim = xlim,
    ylim = ylim
  ) +

  theme_bw() +

  theme(
    panel.grid.minor = element_blank(),

    # --------------------------------------------------------
    # Remove x-axis completely
    # --------------------------------------------------------
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.line.x = element_blank(),

    # --------------------------------------------------------
    # Remove y-axis completely
    # --------------------------------------------------------
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.y = element_blank()
  )


# ------------------------------------------------------------
# 4. Density of p.err.IYC
# ------------------------------------------------------------

p.iyc <- ggplot(
  dat,
  aes(x = p.err.IYC)
) +

  geom_histogram(
    fill = "grey70",
    colour = "black",
    alpha = 0.7,
    linewidth = 0.7
  ) +

  # Rotate the density so that p.err.IYC is vertical
  coord_flip(
    xlim = ylim
  ) +

  labs(
    x = NULL,
    y = "p.err.IYC"
  ) +

  theme_bw() +

  theme(
    panel.grid.minor = element_blank(),

    # --------------------------------------------------------
    # Remove the horizontal density scale
    # --------------------------------------------------------
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank(),

    # --------------------------------------------------------
    # Keep vertical p.err.IYC axis
    # --------------------------------------------------------
    axis.text.y = element_text(size = 10),
    axis.ticks.y = element_line(),
    axis.title.y = element_text(
      size = 12,
      angle = 90
    )
  )
# ------------------------------------------------------------
# 5. Density of SSB2028
# ------------------------------------------------------------

p.ssb <- ggplot(
  dat,
  aes(x = SSB2028)
) +

  geom_histogram(
    fill = "grey70",
    colour = "black",
    alpha = 0.7,
    linewidth = 0.7
  ) +

  coord_cartesian(
    xlim = xlim
  ) +

  labs(
    x = "SSB2028",
    y = NULL
  ) +

  theme_bw() +

  theme(
    panel.grid.minor = element_blank(),

    # Remove y-axis
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.y = element_blank(),

    # Keep x-axis
    axis.title.x = element_text(size = 12),
    axis.text.x = element_text(size = 10)
  )


# ------------------------------------------------------------
# 6. Empty bottom-left panel
# ------------------------------------------------------------

p.empty <- ggplot() +
  theme_void()


# ------------------------------------------------------------
# 7. Combine plots
# ------------------------------------------------------------

figure <-
  (p.iyc | p.main) /
  (p.empty | p.ssb) +

  plot_layout(
    widths = c(0.8, 4),
    heights = c(4, 1.2)
  )


# ------------------------------------------------------------
# 8. Display figure
# ------------------------------------------------------------

figure


# ------------------------------------------------------------
# 9. Save figure
# ------------------------------------------------------------

ggsave(
  "./output/sensSTF/SSB2028_p.err.IYC_density.png",
  figure,
  width = 9,
  height = 7,
  dpi = 300
)
















# ------------------------------------------------------------
# 2. Define common axis limits
# ------------------------------------------------------------

xlim <- range(dat$SSB2028)
ylim <- range(dat$p.err.IYC)


# ------------------------------------------------------------
# 3. Main scatterplot
# ------------------------------------------------------------

p.main <- ggplot(
  dat,
  aes(x = SSB2028, y = p.err.IYC)
) +

  geom_point(
    size = 2,
    alpha = 0.6
  ) +

  # Linear regression
  geom_smooth(
    method = "lm",
    se = TRUE,
    linewidth = 0.8
  ) +

  # Keep the same ranges as the marginal densities
  coord_cartesian(
    xlim = xlim,
    ylim = ylim
  ) +

  theme_bw() +

  theme(
    panel.grid.minor = element_blank(),

    # --------------------------------------------------------
    # Remove x-axis completely
    # --------------------------------------------------------
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.line.x = element_blank(),

    # --------------------------------------------------------
    # Remove y-axis completely
    # --------------------------------------------------------
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.y = element_blank()
  )


# ------------------------------------------------------------
# 4. Density of p.err.IYC
# ------------------------------------------------------------

p.iyc <- ggplot(
  dat,
  aes(x = p.err.IYC)
) +

  geom_histogram(
    fill = "grey70",
    colour = "black",
    alpha = 0.7,
    linewidth = 0.7
  ) +

  # Rotate the density so that p.err.IYC is vertical
  coord_flip(
    xlim = ylim
  ) +

  labs(
    x = NULL,
    y = "p.err.IYC"
  ) +

  theme_bw() +

  theme(
    panel.grid.minor = element_blank(),

    # --------------------------------------------------------
    # Remove the horizontal density scale
    # --------------------------------------------------------
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank(),

    # --------------------------------------------------------
    # Keep vertical p.err.IYC axis
    # --------------------------------------------------------
    axis.text.y = element_text(size = 10),
    axis.ticks.y = element_line(),
    axis.title.y = element_text(
      size = 12,
      angle = 90
    )
  )
# ------------------------------------------------------------
# 5. Density of SSB2028
# ------------------------------------------------------------

p.ssb <- ggplot(
  dat,
  aes(x = SSB2028)
) +

  geom_histogram(
    fill = "grey70",
    colour = "black",
    alpha = 0.7,
    linewidth = 0.7
  ) +

  coord_cartesian(
    xlim = xlim
  ) +

  labs(
    x = "SSB2028",
    y = NULL
  ) +

  theme_bw() +

  theme(
    panel.grid.minor = element_blank(),

    # Remove y-axis
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.y = element_blank(),

    # Keep x-axis
    axis.title.x = element_text(size = 12),
    axis.text.x = element_text(size = 10)
  )


# ------------------------------------------------------------
# 6. Empty bottom-left panel
# ------------------------------------------------------------

p.empty <- ggplot() +
  theme_void()


# ------------------------------------------------------------
# 7. Combine plots
# ------------------------------------------------------------

figure <-
  (p.iyc | p.main) /
  (p.empty | p.ssb) +

  plot_layout(
    widths = c(0.8, 4),
    heights = c(4, 1.2)
  )


# ------------------------------------------------------------
# 8. Display figure
# ------------------------------------------------------------

figure


# ------------------------------------------------------------
# 9. Save figure
# ------------------------------------------------------------

ggsave(
  "./output/sensSTF/SSB2028_p.err.IYC_density.png",
  figure,
  width = 9,
  height = 7,
  dpi = 300
)


















# ------------------------------------------------------------
# 2. Define common axis limits
# ------------------------------------------------------------

xlim <- range(dat$C2027)
ylim <- range(dat$p.err.IYC)


# ------------------------------------------------------------
# 3. Main scatterplot
# ------------------------------------------------------------

p.main <- ggplot(
  dat,
  aes(x = C2027, y = p.err.IYC)
) +

  geom_point(
    size = 2,
    alpha = 0.6
  ) +

  # Linear regression
  geom_smooth(
    method = "lm",
    se = TRUE,
    linewidth = 0.8
  ) +

  # Keep the same ranges as the marginal densities
  coord_cartesian(
    xlim = xlim,
    ylim = ylim
  ) +

  theme_bw() +

  theme(
    panel.grid.minor = element_blank(),

    # --------------------------------------------------------
    # Remove x-axis completely
    # --------------------------------------------------------
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.line.x = element_blank(),

    # --------------------------------------------------------
    # Remove y-axis completely
    # --------------------------------------------------------
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.y = element_blank()
  )


# ------------------------------------------------------------
# 4. Density of p.err.IYC
# ------------------------------------------------------------

p.iyc <- ggplot(
  dat,
  aes(x = p.err.IYC)
) +

  geom_histogram(
    fill = "grey70",
    colour = "black",
    alpha = 0.7,
    linewidth = 0.7
  ) +

  # Rotate the density so that p.err.IYC is vertical
  coord_flip(
    xlim = ylim
  ) +

  labs(
    x = NULL,
    y = "p.err.IYC"
  ) +

  theme_bw() +

  theme(
    panel.grid.minor = element_blank(),

    # --------------------------------------------------------
    # Remove the horizontal density scale
    # --------------------------------------------------------
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank(),

    # --------------------------------------------------------
    # Keep vertical p.err.IYC axis
    # --------------------------------------------------------
    axis.text.y = element_text(size = 10),
    axis.ticks.y = element_line(),
    axis.title.y = element_text(
      size = 12,
      angle = 90
    )
  )
# ------------------------------------------------------------
# 5. Density of C2027
# ------------------------------------------------------------

p.ssb <- ggplot(
  dat,
  aes(x = C2027)
) +

  geom_histogram(
    fill = "grey70",
    colour = "black",
    alpha = 0.7,
    linewidth = 0.7
  ) +

  coord_cartesian(
    xlim = xlim
  ) +

  labs(
    x = "C2027",
    y = NULL
  ) +

  theme_bw() +

  theme(
    panel.grid.minor = element_blank(),

    # Remove y-axis
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.y = element_blank(),

    # Keep x-axis
    axis.title.x = element_text(size = 12),
    axis.text.x = element_text(size = 10)
  )


# ------------------------------------------------------------
# 6. Empty bottom-left panel
# ------------------------------------------------------------

p.empty <- ggplot() +
  theme_void()


# ------------------------------------------------------------
# 7. Combine plots
# ------------------------------------------------------------

figure <-
  (p.iyc | p.main) /
  (p.empty | p.ssb) +

  plot_layout(
    widths = c(0.8, 4),
    heights = c(4, 1.2)
  )


# ------------------------------------------------------------
# 8. Display figure
# ------------------------------------------------------------

figure


# ------------------------------------------------------------
# 9. Save figure
# ------------------------------------------------------------

ggsave(
  "./output/sensSTF/C2027_p.err.IYC_density.png",
  figure,
  width = 9,
  height = 7,
  dpi = 300
)






# ------------------------------------------------------------
# 2. Define common axis limits
# ------------------------------------------------------------

xlim <- range(dat$pBlim2028)
ylim <- range(dat$p.err.IYC)


# ------------------------------------------------------------
# 3. Main scatterplot
# ------------------------------------------------------------

p.main <- ggplot(
  dat,
  aes(x = pBlim2028, y = p.err.IYC)
) +

  geom_point(
    size = 2,
    alpha = 0.6
  ) +

  # Linear regression
  geom_smooth(
    method = "lm",
    se = TRUE,
    linewidth = 0.8
  ) +

  # Keep the same ranges as the marginal densities
  coord_cartesian(
    xlim = xlim,
    ylim = ylim
  ) +

  theme_bw() +

  theme(
    panel.grid.minor = element_blank(),

    # --------------------------------------------------------
    # Remove x-axis completely
    # --------------------------------------------------------
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.line.x = element_blank(),

    # --------------------------------------------------------
    # Remove y-axis completely
    # --------------------------------------------------------
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.y = element_blank()
  )


# ------------------------------------------------------------
# 4. Density of p.err.IYC
# ------------------------------------------------------------

p.iyc <- ggplot(
  dat,
  aes(x = p.err.IYC)
) +

  geom_histogram(
    fill = "grey70",
    colour = "black",
    alpha = 0.7,
    linewidth = 0.7
  ) +

  # Rotate the density so that p.err.IYC is vertical
  coord_flip(
    xlim = ylim
  ) +

  labs(
    x = NULL,
    y = "p.err.IYC"
  ) +

  theme_bw() +

  theme(
    panel.grid.minor = element_blank(),

    # --------------------------------------------------------
    # Remove the horizontal density scale
    # --------------------------------------------------------
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank(),

    # --------------------------------------------------------
    # Keep vertical p.err.IYC axis
    # --------------------------------------------------------
    axis.text.y = element_text(size = 10),
    axis.ticks.y = element_line(),
    axis.title.y = element_text(
      size = 12,
      angle = 90
    )
  )
# ------------------------------------------------------------
# 5. Density of pBlim2028
# ------------------------------------------------------------

p.ssb <- ggplot(
  dat,
  aes(x = pBlim2028)
) +

  geom_histogram(
    fill = "grey70",
    colour = "black",
    alpha = 0.7,
    linewidth = 0.7
  ) +

  coord_cartesian(
    xlim = xlim
  ) +

  labs(
    x = "pBlim2028",
    y = NULL
  ) +

  theme_bw() +

  theme(
    panel.grid.minor = element_blank(),

    # Remove y-axis
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.y = element_blank(),

    # Keep x-axis
    axis.title.x = element_text(size = 12),
    axis.text.x = element_text(size = 10)
  )


# ------------------------------------------------------------
# 6. Empty bottom-left panel
# ------------------------------------------------------------

p.empty <- ggplot() +
  theme_void()


# ------------------------------------------------------------
# 7. Combine plots
# ------------------------------------------------------------

figure <-
  (p.iyc | p.main) /
  (p.empty | p.ssb) +

  plot_layout(
    widths = c(0.8, 4),
    heights = c(4, 1.2)
  )


# ------------------------------------------------------------
# 8. Display figure
# ------------------------------------------------------------

figure


# ------------------------------------------------------------
# 9. Save figure
# ------------------------------------------------------------

ggsave(
  "./output/sensSTF/pBlim2028_p.err.IYC_density.png",
  figure,
  width = 9,
  height = 7,
  dpi = 300
)































