library(patchwork)
library(grid)

pls <- Map(function(x, y) plot(x) + ggtitle(y), x=Forage,
           y=c("Prior", "h=0.9", "Depensation", rep("", 7)))

pls <- lapply(setNames(pls, nm=LETTERS[seq(pls)]), function(x) x + theme(
  plot.margin = margin(t = 0, r = 0, b = 0, l = 0)) + ylim(0, 3.5))



wrap_elements(panel = textGrob('Reference', rot=90)) +
  pls[[1]] + pls[[2]] + pls[[3]] +
  wrap_elements(panel = textGrob('Env. driven', rot=90)) +
  pls[[4]] +
  wrap_elements(panel = textGrob('High M', rot=90)) +
  pls[[5]] +
  wrap_elements(panel = textGrob('Regime', rot=90)) +
  wrap_elements(panel = textGrob('Reference', rot=90)) +
  pls[[6]] + pls[[7]] + pls[[8]] +
  wrap_elements(panel = textGrob('Env. driven', rot=90)) +
  pls[[9]] +
  wrap_elements(panel = textGrob('High M', rot=90)) +
  pls[[10]] +
  wrap_elements(panel = textGrob('AR', rot=90)) +
  plot_layout(design="
  IABBBCCCDDD
  #EFFF######
  #GHHH######
  SJKKKLLLMMM
  #OPPP######
  #QRRR######
  ")