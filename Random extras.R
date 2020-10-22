mean(top25sector$media), mean(top25sector$academia),  mean(top25sector$organization) + mean(top25sector$business) + mean(top25sector$public) + mean(top25sector$decline)


mean(top25sector$media)
mean(top25sector$academia)
mean(top25sector$organization)
mean(top25sector$business)
mean(top25sector$public)
mean(top25sector$decline)

rlang::last_error()


abandonment <- top25ag %>% 
  mutate(abandonment = abandoned_in_rp/closed_in_rp) %>% 
  select(agency_name, abandonment)

top25ag$s13.s15rate <- top25ag$s13_rate + top25ag$s15_rate
