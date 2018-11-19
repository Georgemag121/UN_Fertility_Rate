
# Back to 8324
dup1 <- tfr1950 %>% mutate(recal = exp(RecallLag*a), dhs = (DataProcess == "DHS-NS" | DataProcess == "DHS" | DataProcess == "DHS/MICS" | DataProcess == "DHS/PAPFAM"), direct = (DataTypeGroupName == "Direct" | DataTypeGroupName == "UN computed")) %>% group_by(Country.or.area, year) %>% mutate(dup = n(), tot_dir = sum(direct), tot_dhs = sum(dhs)) %>% ungroup() %>% filter(dup > 1)

# 4930 with no dups group 1 and 2
group1 <- tfr1950 %>% mutate(recal = exp(RecallLag*a), dhs = (DataProcess == "DHS-NS" | DataProcess == "DHS" | DataProcess == "DHS/MICS" | DataProcess == "DHS/PAPFAM"), direct = (DataTypeGroupName == "Direct" | DataTypeGroupName == "UN computed")) %>% group_by(Country.or.area, year) %>% mutate(dup = n(), tot_dir = sum(direct)) %>% ungroup() %>% filter(dup == 1) %>% mutate(tfr_new = DataValue) %>% mutate(group.num = direct + 1) %>% distinct(ISO.code, Country.or.area, year, tfr_new, group.num) 

# 516 dups with only one direct
group3 <- dup1 %>% filter(tot_dir == 1, direct == TRUE) %>% mutate(tfr_new = DataValue) %>% mutate(group.num = 3) %>% distinct(ISO.code, Country.or.area, year, tfr_new, group.num) 

# 781 dups with no direct, weighted avg of indirect
group4 <- dup1 %>% filter(tot_dir == 0) %>% group_by(Country.or.area, year) %>% mutate(tfr_new = weighted.mean(DataValue, w = exp(RecallLag))) %>% ungroup() %>%  mutate(group.num = 4) %>% distinct(ISO.code, Country.or.area, year, tfr_new, group.num) 

# 799 dups with multiple direct and contain dhs sources
group5 <- dup1 %>% filter(tot_dir >= 2,  tot_dhs >= 1, direct == TRUE | dhs == TRUE) %>% group_by(Country.or.area, year) %>% mutate(tfr_new = weighted.mean(DataValue, w = exp(RecallLag))) %>% ungroup() %>% mutate(group.num = 5) %>% distinct(ISO.code, Country.or.area, year, tfr_new, group.num) 

# 5548 dups with multiple direct and no dhs sources
group6 <- dup1 %>% filter(tot_dir >= 2, direct == TRUE, tot_dhs == 0) %>% group_by(Country.or.area, year) %>% mutate(tfr_new = weighted.mean(DataValue, w = exp(RecallLag))) %>% ungroup() %>% mutate(group.num = 6) %>% distinct(ISO.code, Country.or.area, year, tfr_new, group.num) 


# Combine 8322, 
tfr_adjusted <- rbind(group1, group3, group4, group5, group6)
tfr_adjusted <- tfr_adjusted %>% arrange(Country.or.area, year) %>% mutate(indirect = (group.num == 1 | group.num == 4)) %>% rename(DataValue = tfr_new)

#View(dup1[which(dup1$dhs == TRUE & dup1$DataTypeGroupName == "Indirect"), ])

tfr_adjusted %>% filter(Country.or.area == "Zambia") %>% ggplot(aes(x = year, y = tfr_new)) + geom_point(aes(col = indirect)) + geom_line() + lims(x = c(1950,2016)) + scale_y_continuous(limits = c(0, 11.5))

par(mfrow = c(1,2))
best_model("Afghanistan", df = tfr_m)$plot
best_model("Afghanistan", df = tfr_adjusted)$plot
