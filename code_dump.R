SSB_full_renamed <- SSB_full %>%
  rename(
    SSB_component = SSB,
    l_bnd_component = l_bnd,
    u_bnd_component = u_bnd
  ) %>%
  mutate(component = "Full Stock")

SSB_combined <- bind_rows(SSB_LAI, SSB_full_renamed)

plot_SSB_combined <- ggplot(SSB_combined, aes(x = year, y = SSB_component / 1e6, color = component, fill = component)) +
  geom_ribbon(aes(ymin = l_bnd_component / 1e6,
                  ymax = u_bnd_component / 1e6), alpha = 0.3, color = NA) +
  geom_line(linewidth = 0.9) +
  labs(title = "SSB with 95% Confidence Bounds (Components + Full Stock)",
       x = "Year",
       y = "SSB in million tonnes") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10)))

plot_SSB_combined