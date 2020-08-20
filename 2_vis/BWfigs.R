# Black and white figures for presentation

# Main figures ------------------------------------------------------------

source('~/Documents/GitHub/foodwaste/fwe/figs/theme_black.R')

theme_set(theme_black() +
            theme(panel.grid.major.x = element_blank(),
                  panel.grid.major.y = element_blank(),
                  panel.grid.minor.x = element_blank(),
                  panel.grid.minor.y = element_blank(),
                  axis.text.x = element_blank(),
                  axis.title.x = element_blank(),
                  axis.ticks.x = element_blank(),
                  strip.background = element_blank(),
                  legend.position = 'none'))

fig0 <- ggplot(dat_netcost_alternate %>% mutate_if(is.numeric, ~ ./1e6), aes(x = intervention, y = q50, color = name)) +
  geom_hline(yintercept = 0, linetype = 'dotted', color = 'gray75') +
  geom_errorbar(aes(ymin = q05, ymax = q95), size = 2, alpha = 0.5, width = 0, position = pd) +
  geom_point(size = 2, position = pd) +
  geom_errorbar(aes(ymin = q025, ymax = q975), width = 0.05, position = pd) +
  scale_x_discrete(labels = c('CEC', 'SPP', 'SDL', 'WTA')) +
  scale_y_continuous(name = 'Total annual cost (million $)', expand = expansion(mult = 0.01)) +
  ggsci::scale_color_nejm(labels = c('Averted food purchases', 'Net cost or savings', 'Cost of implementation')) +
  theme(axis.text.x = element_text(color = 'white'), legend.position = c(0.75, 0.2), legend.title = element_blank(),
        axis.text.y = element_text(color = c(rep('forestgreen',3),'white')),
        legend.key = element_blank())

fig1 <- ggplot(dat_netaverted_alternate, aes(x = intervention, color = intervention, y = q50, ymin = q025, ymax = q975)) +
  facet_wrap(~ category_labels, scales = 'free', labeller = label_parsed) +
  geom_segment(aes(xend = intervention, y = q05, yend = q95), size = 2, alpha = 0.5) +
  geom_point(size = 2) +
  geom_blank(data = dummy_dat) +
  geom_errorbar(width = 0.05) +
  scale_x_discrete(labels = c('CEC', 'SPP', 'SDL', 'WTA')) +
  scale_y_continuous(name = 'Net impact averted', expand = expansion(mult = c(0, 0.05))) +
  theme(axis.text.x = element_text(size = 6, color = 'white')) +
  interv_colors 

fig2 <- ggplot(dat_unitcost_alternate, aes(x = intervention, color = intervention, y = 1/q50, ymin = 1/q025, ymax = 1/q975)) +
  facet_wrap(~ costeff_labels, scales = 'free', labeller = label_parsed) +
  geom_segment(aes(xend = intervention, y = 1/q05, yend = 1/q95), size = 2, alpha = 0.5) +
  geom_point(size = 2) +
  geom_blank(data = dummy_dat, aes(y = 0, ymin = 0, ymax = 0, yend = 0)) +
  geom_errorbar(width = 0.05) +
  scale_x_discrete(labels = c('CEC', 'SPP', 'SDL', 'WTA')) +
  scale_y_continuous(name = 'Reduction per $1 spent',expand = expansion(mult = c(0, 0.05))) +
  theme(axis.text.x = element_text(size = 6, color = 'white')) +
  interv_colors 

# Additional figure with cost breakdown between annual and annualized.

fig0b_data <- dat_costbreakdown_alternate %>% 
  ungroup %>%
  filter(!cost_type %in% 'initial_raw') 

spp_total <- dat_totalcost_alternate %>% 
  mutate_if(is.numeric, ~ ./1e6) %>%
  filter(intervention=='spoilage prevention packaging') %>%
  mutate(cost_type = 'total')

# Add row for total costs in SPP.
fig0b_data <- bind_rows(fig0b_data, spp_total)


pd <- position_dodge(width = 0.1)
fig0b <- ggplot(fig0b_data, aes(x = intervention, y = q50, color = cost_type)) +
  geom_errorbar(aes(ymin = q05, ymax = q95), size = 2, alpha = 0.5, width = 0, position = pd) +
  geom_point(size = 2, position = pd) +
  geom_errorbar(aes(ymin = q025, ymax = q975), width = 0.05, position = pd) +
  scale_x_discrete(labels = c('CEC', 'SPP', 'SDL', 'WTA')) +  
  scale_y_continuous(name = 'Annual cost (million $)', expand = c(0, 0), limits = c(0, 825)) +
  scale_color_manual(labels = c('Annual', 'Initial (annualized)', 'Total'), values = c('forestgreen', 'goldenrod', 'plum')) +
  theme(axis.text.x = element_text(color = 'white'), 
        axis.title.x = element_text(), 
        legend.position = c(0.2, 0.87),
        legend.title = element_blank(),
        legend.key = element_blank(),
        legend.background = element_rect(color = NA, fill = 'transparent'))



fp_fig <- '~/Dropbox/Q/presentations/rti2020'

ggsave(file.path(fp_fig, 'fig_averted.png'), fig1, height = 4, width = 5.25, dpi = 400)
ggsave(file.path(fp_fig, 'fig_costeff.png'), fig2, height = 4, width = 5.25, dpi = 400)
ggsave(file.path(fp_fig, 'fig_cost.png'), fig0, height = 4, width = 5.25, dpi = 400)
ggsave(file.path(fp_fig, 'fig_costbreakdown.png'), fig0b, height = 4, width = 5.25, dpi = 400)

# Combine the two fig0 into a single figure with same dimensions

library(grid)

fig0_combined <- cbind(ggplotGrob(fig0b), ggplotGrob(fig0))
png(file.path(fp_fig, 'fig_combined_cost.png'), height = 4, width = 10, res = 400, units = 'in')
grid.draw(fig0_combined)
dev.off()
