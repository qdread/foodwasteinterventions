# Publication-quality figures.
# QDR / Foodwaste interventions / 04 Aug 2020

# Figure 1 : p_totalimpact_alternate
# Figure 2 : p_costeff_alternate 
# Supplemental figures:
# 1 is net cost and differences for SPP intervention by food type
# 2 is impact reduction for SPP by food type
# 3 is cost effectiveness for SPP by food type
# 4 is refed comparison of total cost
# 5 is refed comparison of GHG
# 6 is refed comparison of water use


# Main figures ------------------------------------------------------------

# Color scheme set by RTI editor
rti_colors <- scale_color_manual(values = c('#8dc63f', '#d73642', '#6b489d', '#00aae7'), guide = guide_legend(nrow = 2))

theme_set(theme_bw() +
            theme(panel.grid.major.x = element_blank(),
                  panel.grid.minor.x = element_blank(),
                  axis.text.x = element_blank(),
                  axis.title.x = element_blank(),
                  axis.ticks.x = element_blank(),
                  strip.background = element_blank(),
                  legend.position = 'none'))

p_totalimpact_alternate <- ggplot(dat_netaverted_alternate, aes(x = intervention, color = intervention, y = q50, ymin = q025, ymax = q975)) +
  facet_wrap(~ category_labels, scales = 'free', labeller = label_parsed) +
  geom_segment(aes(xend = intervention, y = q05, yend = q95), size = 2, alpha = 0.5) +
  geom_point(size = 2) +
  geom_blank(data = dummy_dat) +
  geom_errorbar(width = 0.05) +
  scale_x_discrete(labels = c('CEC', 'SPP', 'SDL', 'WTA')) +
  scale_y_continuous(name = 'Net impact averted', expand = expansion(mult = c(0, 0.05))) +
  theme(axis.text.x = element_text(size = 6), legend.position = 'none') +
  interv_colors 

p_costeff_alternate <- ggplot(dat_unitcost_alternate, aes(x = intervention, color = intervention, y = 1/q50, ymin = 1/q025, ymax = 1/q975)) +
  facet_wrap(~ costeff_labels, scales = 'free', labeller = label_parsed) +
  geom_segment(aes(xend = intervention, y = 1/q05, yend = 1/q95), size = 2, alpha = 0.5) +
  geom_point(size = 2) +
  geom_blank(data = dummy_dat, aes(y = 0, ymin = 0, ymax = 0, yend = 0)) +
  geom_errorbar(width = 0.05) +
  scale_x_discrete(labels = c('CEC', 'SPP', 'SDL', 'WTA')) +
  scale_y_continuous(name = 'Reduction per $1 spent',expand = expansion(mult = c(0, 0.05))) +
  theme(axis.text.x = element_text(size = 6), legend.position = 'none') +
  interv_colors 

theme_maintext <- theme_bw() +
  theme(axis.text.x = element_text(size = 9, color = 'black'),
        axis.text.y = element_text(size = 9, color = 'black'),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        strip.background = element_blank(),
        legend.position = 'none',
        panel.grid = element_blank(),
         text = element_text(color = 'black'))

fig1 <- p_totalimpact_alternate + 
  geom_text(data = data.frame(category_labels = sort(unique(dat_netaverted_alternate$category_labels)),
                              letter = letters[1:4],
                              intervention = NA, q025 = 0, q50 = 0, q975 = 0),
            aes(label = letter),
            x = -Inf, y = Inf, hjust = -0.1, vjust = 1.1, color = 'black', fontface = 'bold', size = 6) +
  theme_maintext + rti_colors

fig2 <- p_costeff_alternate + 
  geom_text(data = data.frame(costeff_labels = sort(unique(dat_unitcost_alternate$costeff_labels)),
                              letter = letters[1:4],
                              intervention = NA, q025 = 0, q50 = 0, q975 = 0),
            aes(label = letter),
            x = -Inf, y = Inf, hjust = -0.1, vjust = 1.1, color = 'black', fontface = 'bold', size = 6) +
  theme_maintext + rti_colors

fp_fig <- '.'
W <- 1656
H <- 1209
ggsave(file.path(fp_fig, 'fig1.png'), fig1, height = H/300, width = W/300, dpi = 300)
ggsave(file.path(fp_fig, 'fig2.png'), fig2, height = H/300, width = W/300, dpi = 300)

# Supplemental figures ----------------------------------------------------

dark_colors <- RColorBrewer::brewer.pal(8, 'Dark2')
food_colors <- scale_color_manual(values = dark_colors[c(4,7,6,2,5)])
food_colors <- scale_color_manual(values = c('#6b489d','#806213', '#bf931c','#f58021', '#8dc63f'))

pos_dodge <- position_dodge(width = 0.2)

p_pkgnetcostbyfood <- ggplot(pkg_netcost_byfood %>% mutate(name = if_else(name == 'annualized_total_cost', 'total_cost', name)), aes(x = food, y = q50, color = name)) +
  geom_hline(yintercept = 0, linetype = 'dotted') +
  geom_errorbar(aes(ymin = q05, ymax = q95), size = 2, alpha = 0.5, width = 0, position = pos_dodge) +
  geom_point(size = 2, position = pos_dodge) +
  geom_errorbar(aes(ymin = q025, ymax = q975), width = 0.05, position = pos_dodge) +
  scale_y_continuous(name = 'Cost (million $)', expand = expansion(mult = 0.01)) +
  scale_color_manual(values = c('#d73642', '#00aae7', '#b5931c'),
                     labels = c('Averted food purchases', 'Net cost or savings', 'Cost of implementation'),
                     guide = guide_legend(nrow = 2)) +
  theme_withxaxis +
  theme(legend.position = 'bottom', legend.title = element_blank(),
        axis.text.x = element_text(color = 'black'),
        axis.text.y = element_text(color = c('forestgreen','forestgreen','black')),
        axis.title.x = element_blank(),
        panel.grid = element_blank(),
        axis.ticks.x = element_blank())

p_pkgimpactbyfood <- ggplot(pkg_averted_byfood %>% filter(!food %in% 'misc'), aes(x = food, color = food, y = q50, ymin = q025, ymax = q975)) +
  facet_wrap(~ category_labels, scales = 'free_y', labeller = label_parsed) +
  geom_segment(aes(xend = food, y = q05, yend = q95), size = 2, alpha = 0.5) +
  geom_point(size = 2) +
  geom_errorbar(width = 0.05) +
  scale_y_continuous(name = 'Net impact averted', expand = expansion(mult = c(0, 0.05))) +
  theme_withxaxis +
  theme(axis.title.x = element_blank(), legend.position = 'none', axis.text.x = element_text(size = 7, color = 'black'),
        axis.text.y = element_text(color = 'black'),
        panel.grid = element_blank(),
        axis.ticks.x = element_blank()) +
  food_colors +
  geom_text(data = data.frame(category_labels = sort(unique(pkg_averted_byfood$category_labels)),
                              letter = letters[1:4],
                              food = NA, q025 = 0, q50 = 0, q975 = 0),
            aes(label = letter),
            x = -Inf, y = Inf, hjust = -0.1, vjust = 1.1, color = 'black', fontface = 'bold', size = 6)

p_pkgcosteffbyfood <- ggplot(pkg_cost_byfood %>% filter(!food %in% 'misc'), aes(x = food, color = food, y = 1/q50, ymin = 1/q025, ymax = 1/q975)) +
  facet_wrap(~ costeff_labels, scales = 'free_y', labeller = label_parsed) +
  geom_segment(aes(xend = food, y = 1/q05, yend = 1/q95), size = 2, alpha = 0.5) +
  geom_point(size = 2) +
  geom_errorbar(width = 0.05) +
  scale_y_continuous(name = 'Reduction per $1 spent', expand = expansion(mult = c(0, 0.05))) +
  theme_withxaxis +
  theme(axis.title.x = element_blank(), legend.position = 'none', axis.text.x = element_text(size = 7, color = 'black'),
        axis.text.y = element_text(color = 'black'),
        panel.grid = element_blank(),
        axis.ticks.x = element_blank()) +
  food_colors +
  geom_text(data = data.frame(costeff_labels = sort(unique(pkg_cost_byfood$costeff_labels)),
                                         letter = letters[1:4],
                                         food = NA, q025 = 0, q50 = 0, q975 = 0),
                       aes(label = letter),
                       x = -Inf, y = Inf, hjust = -0.1, vjust = 1.1, color = 'black', fontface = 'bold', size = 6)


# Refed supp figs ---------------------------------------------------------

theme_set(theme_bw() +
            theme(panel.grid = element_blank(),
                  legend.position = 'none',
                  axis.text.x = element_text(color = 'black'),
                  axis.text.y = element_text(color = 'black'),
                  axis.title.x = element_blank(),
                  axis.ticks.x = element_blank()))

p_totalcost <- ggplot(all_cost_table %>% mutate(intervention = str_wrap(intervention, width = 25)), aes(x = intervention, color = intervention)) +
  geom_point(aes(y = ReFED_estimate), pch = 1, size = 4, color = 'black', stroke = .8) +
  geom_segment(aes(xend = intervention, y = q05, yend = q95), size = 2, alpha = 0.5) +
  geom_point(aes(y = q50), size = 2) +
  geom_errorbar(aes(ymin = q025, ymax = q975), width = 0.05) +  scale_y_continuous(name = 'Total annual cost (million $)') +
  rti_colors

p_ghgaverted <- ggplot(all_ghg_table %>% mutate(intervention = str_wrap(intervention, width = 25)), aes(x = intervention, color = intervention)) +
  geom_point(aes(y = ReFED_estimate), pch = 1, size = 4, color = 'black', stroke = .8) +
  geom_segment(aes(xend = intervention, y = q05, yend = q95), size = 2, alpha = 0.5) +
  geom_point(aes(y = q50), size = 2) +
  geom_errorbar(aes(ymin = q025, ymax = q975), width = 0.05) +
  scale_y_continuous(name = parse(text = 'Potential~GHG~emissions~reduced~(MT~CO[2])')) +
  rti_colors

p_wateraverted <- ggplot(all_h2o_table %>% mutate(intervention = str_wrap(intervention, width = 25)), aes(x = intervention, color = intervention)) +
  geom_point(aes(y = as.numeric(ReFED_estimate)), pch = 1, size = 4, color = 'black', stroke = .8) +
  geom_segment(aes(xend = intervention, y = q05, yend = q95), size = 2, alpha = 0.5) +
  geom_point(aes(y = q50), size = 2) +
  geom_errorbar(aes(ymin = q025, ymax = q975), width = 0.05) +
  scale_y_continuous(name = parse(text = 'Potential~water~use~reduced~(km^3)')) +
  rti_colors

W2 <- 5.75
ggsave(file.path(fp_fig, 'figS1.png'), p_pkgnetcostbyfood, height = 4, width = W2, dpi = 400)
ggsave(file.path(fp_fig, 'figS2.png'), p_pkgimpactbyfood, height = 4, width = W2, dpi = 400)
ggsave(file.path(fp_fig, 'figS3.png'), p_pkgcosteffbyfood, height = 4, width = W2, dpi = 400)
ggsave(file.path(fp_fig, 'figS4.png'), p_totalcost, height = 4, width = W2, dpi = 400)
ggsave(file.path(fp_fig, 'figS5.png'), p_ghgaverted, height = 4, width = W2, dpi = 400)
ggsave(file.path(fp_fig, 'figS6.png'), p_wateraverted, height = 4, width = W2, dpi = 400)
