#Access data from the openFEC api using R wrapper from robinspollak. Uncomment lines 2 and 3 to install R.openFEC package.
#library(devtools)
#install_github("robinspollak/R.openFEC")

#Load libraries needed to run this script.
library(R.openFEC)
library(httr)
library(tidyverse)

#signup for api key at https://api.open.fec.gov/developers/. Save a one line file called "data.gov.key" in the root project folder, that one line assigning the key to a variable like the next line:
# api_key <- "KEYSTRINGFROMFEC"
source("data.gov.key") 


#Set params for the election we are interested in.
election_of_interest <- list(
  state = "VA",
  office_abb = "H",
  office_full = "house",
  district = "05",
  cycle = "2018",
  min_raised = 20000,
  candidate_status = c("C") #Tested ptions are C for current candidate, P for prior, 
  )



#Tag yourself for if anyone has questions
author <- "@StephenHolz"

#Create path and folder for saving charts
output_path <- file.path("output",election_of_interest$cycle,election_of_interest$state,election_of_interest$office_full,election_of_interest$district)
dir.create(output_path,recursive = TRUE)


#Find current candidates so we can get IDs necessary for more detailed financial info and filter on current status.
#If there are more candidates than 20 currently in the race, you'll need to do this several times changing the page parameter or increase results per page.
candidates_info <- candidates_search(api_key,
                                     query_params = list(
                                       state = election_of_interest$state,
                                       office = election_of_interest$office,
                                       district = election_of_interest$district,
                                       cycle = election_of_interest$cycle,
                                       page="1"#,
                                       #candidate_status = election_of_interest$candidate_status)
                                     )
)

for(i in 1:length(candidates_info[["content"]][["results"]])){
  if(length(candidates_info[["content"]][["results"]][[i]]) != 2){
    detailed_financial_info[[candidates_df$last_name[i]]] <- NULL
  }else{
    
  }
}

candidates_df <- data.frame(
  name = map(candidates_info[["content"]][["results"]], function(x) x$name) %>% unlist(),
  candidate_id = map(candidates_info[["content"]][["results"]], function(x) x$candidate_id) %>% unlist(),
  party = map(candidates_info[["content"]][["results"]], function(x) x$party) %>% unlist(),
  candidate_status = map(candidates_info[["content"]][["results"]], function(x) x$candidate_status) %>% unlist()
) %>%
  separate(name, c('last_name', 'first_name'), sep = ',') %>%
  filter(candidate_status %in% election_of_interest$candidate_status)
###Request Top Level Receipts and Disbursement Info for All Candidates

top_financial_info <- get_election_financial_summary(api_key,
                                                     query_params = list(
                                                       state = election_of_interest$state,
                                                       office = election_of_interest$office_full,
                                                       district = election_of_interest$district,
                                                       cycle = election_of_interest$cycle)
                                                     )

top_financial_df <- data.frame(
  name = map(top_financial_info[["content"]][["results"]], function(x) x$candidate_name) %>% unlist(),
  candidate_id = map(top_financial_info[["content"]][["results"]], function(x) x$candidate_id) %>% unlist(),
  total_receipts = map(top_financial_info[["content"]][["results"]], function(x) x$total_receipts) %>% unlist(),
  total_disbursements = map(top_financial_info[["content"]][["results"]], function(x) x$total_disbursements) %>% unlist(),
  cash_on_hand_end_period = map(top_financial_info[["content"]][["results"]], function(x) x$cash_on_hand_end_period) %>% unlist()
) %>%
  mutate(total_check = total_disbursements + cash_on_hand_end_period) %>%
  left_join(candidates_df) %>%
  filter(candidate_status %in% election_of_interest$candidate_status)

period_ending <- as.factor(map(top_financial_info[["content"]][["results"]], function(x) x$coverage_end_date) %>% unlist())

if(length(levels(period_ending)) != 1){
  stop("There are different period_endings for the candidates returned")
}else{
  period_ending <- substr(levels(period_ending)[1],1,10)
}

colors <- c("DEM" = "#2c4d82", "REP" = "#8e1b1b", "IND" = "#a3a3a3", "GRE" = "#1c561d", "LIB" = "#afac3d")

chart_theme <- theme(
  panel.background = element_blank(),
  panel.grid.major.x = element_line(color = "#7c7c7c",size = .4),
  panel.grid.major.y = element_blank(),
  axis.ticks = element_blank(),
  plot.title = element_text(size = 26),
  plot.subtitle = element_text(size = 18)
)

#Total Receipts

p1 <- ggplot(data = top_financial_df) +
  geom_bar(aes(x = reorder(last_name, total_receipts), y = total_receipts, fill = party), stat = "identity", alpha = .95) +
  geom_text(aes(x = reorder(last_name, total_receipts), y = total_receipts, label = scales::dollar(total_receipts)),hjust = -0.1) +
  scale_y_continuous(labels = scales::dollar,limits = c(0,max(top_financial_df$total_receipts)*1.2)) +
  scale_fill_manual(values = colors) +
  labs(
    title = paste0(election_of_interest$cycle," ",election_of_interest$state,election_of_interest$district," ",str_to_title(election_of_interest$office_full)," Race Total Receipts"),
    subtitle = paste0("Period ending ",period_ending),
    x = "",
    y = "",
    caption = paste0("Source: Federal Election Commmission, data retreived on ",Sys.Date(), " by ",author),
    fill = "Party"
  ) +
  chart_theme + 
  coord_flip()

filename <- paste0(Sys.Date(),"-total-receipts-",election_of_interest$cycle,"-",election_of_interest$state,"-",election_of_interest$office_full,"-",election_of_interest$district,".png")
ggsave(filename = filename, plot = p1, path = output_path, width = 10, height = 6, unit = "in")

#Total Disbursements
p2 <- ggplot(data = top_financial_df) +
  geom_bar(aes(x = reorder(last_name, total_disbursements), y = total_disbursements, fill = party), stat = "identity", alpha = .95) +
  geom_text(aes(x = reorder(last_name, total_disbursements), y = total_disbursements, label = scales::dollar(total_disbursements)),hjust = -0.1) +
  scale_y_continuous(labels = scales::dollar,limits = c(0,max(top_financial_df$total_disbursements)*1.2)) +
  scale_fill_manual(values = colors) +
  labs(
    title = paste0(election_of_interest$cycle," ",election_of_interest$state,election_of_interest$district," ",str_to_title(election_of_interest$office_full)," Race Total Disbursements"),
    subtitle = paste0("Period ending ",period_ending),
    x = "",
    y = "",
    caption = paste0("Source: Federal Election Commmission, data retreived on ",Sys.Date(), " by ",author),
    fill = "Party"
  ) +
  chart_theme + 
  coord_flip()

filename <- paste0(Sys.Date(),"-total-disbursements-",election_of_interest$cycle,"-",election_of_interest$state,"-",election_of_interest$office_full,"-",election_of_interest$district,".png")
ggsave(filename = filename, plot = p2, path = output_path, width = 10, height = 6, unit = "in")

#Cash on Hand
p3 <- ggplot(data = top_financial_df) +
  geom_bar(aes(x = reorder(last_name, cash_on_hand_end_period), y = cash_on_hand_end_period, fill = party), stat = "identity", alpha = .95) +
  geom_text(aes(x = reorder(last_name, cash_on_hand_end_period), y = cash_on_hand_end_period, label = scales::dollar(cash_on_hand_end_period)),hjust = -0.1) +
  scale_y_continuous(labels = scales::dollar,limits = c(0,max(top_financial_df$cash_on_hand_end_period)*1.2)) +
  scale_fill_manual(values = colors) +
  labs(
    title = paste0(election_of_interest$cycle," ",election_of_interest$state,election_of_interest$district," ",str_to_title(election_of_interest$office_full)," Race Cash on Hand"),
    subtitle = paste0("Period ending ",period_ending),
    x = "",
    y = "",
    caption = paste0("Source: Federal Election Commmission, data retreived on ",Sys.Date(), " by ",author),
    fill = "Party"
  ) +
  chart_theme + 
  coord_flip()

filename <- paste0(Sys.Date(),"-cash-on-hand-",election_of_interest$cycle,"-",election_of_interest$state,"-",election_of_interest$office_full,"-",election_of_interest$district,".png")
ggsave(filename = filename, plot = p3, path = output_path, width = 10, height = 6, unit = "in")

detailed_financial_info <- list()

##get_totals_for_candidate function, need to make a pull request with R.openFEC.
get_totals_for_candidate <- function(candidate_id, api_key, query_params = list()){
  fec_api(path = paste0("/candidate/", candidate_id, "/totals/"), api_key = api_key, query_params = query_params)
}

for(i in 1:nrow(candidates_df)){
  detailed_financial_info[[candidates_df$last_name[i]]] <- get_totals_for_candidate(
    candidate_id =candidates_df$candidate_id[i],
    api_key = api_key,
    query_params = list(
      state = election_of_interest$state,
      office = election_of_interest$office_abb,
      district = election_of_interest$district,
      cycle = election_of_interest$cycle)
    )
  
}
for(i in 1:nrow(candidates_df)){
  if(length(detailed_financial_info[[candidates_df$last_name[i]]][["content"]][["results"]]) != 2){
    detailed_financial_info[[candidates_df$last_name[i]]] <- NULL
  }else{
    
  }
}

detailed_financial_df <- data.frame(
  candidate_id = map(detailed_financial_info, function(x) x[["content"]][["results"]][[2]]$candidate_id) %>% unlist(),
  individual_itemized_contributions = map(detailed_financial_info, function(x) x[["content"]][["results"]][[2]]$individual_itemized_contributions) %>% unlist(),
  individual_unitemized_contributions = map(detailed_financial_info, function(x) x[["content"]][["results"]][[2]]$individual_unitemized_contributions) %>% unlist(),
  individual_contributions = map(detailed_financial_info, function(x) x[["content"]][["results"]][[2]]$individual_contributions) %>% unlist(),
  political_party_committee_contributions = map(detailed_financial_info, function(x) x[["content"]][["results"]][[2]]$political_party_committee_contributions) %>% unlist(),
  other_political_committee_contributions = map(detailed_financial_info, function(x) x[["content"]][["results"]][[2]]$other_political_committee_contributions) %>% unlist(),
  candidate_contributions = map(detailed_financial_info, function(x) x[["content"]][["results"]][[2]]$candidate_contribution) %>% unlist(),
  other_receipts = map(detailed_financial_info, function(x) x[["content"]][["results"]][[2]]$other_receipts) %>% unlist(),
  loans = map(detailed_financial_info, function(x) x[["content"]][["results"]][[2]]$loans) %>% unlist()
) %>%
  gather(key = "receipt_type", value = "total_receipts",
         individual_itemized_contributions,
         individual_unitemized_contributions,
         political_party_committee_contributions,
         other_political_committee_contributions,
         candidate_contributions,
         other_receipts,
         loans
         ) %>%
  left_join(candidates_df) %>%
  group_by(last_name) %>%
  mutate(percent = total_receipts/sum(total_receipts),
         receipt_type = str_to_title(gsub("_"," ",receipt_type)))

p4 <- ggplot(detailed_financial_df) +
  geom_bar(aes(x = last_name, y = total_receipts, fill = party), position = "stack", stat = "identity") +
  geom_text(aes(x = last_name, y = total_receipts, label = paste0(scales::dollar(round(total_receipts/1000)),"K")),hjust = -0.1) +
  facet_wrap(~receipt_type) +
  theme(legend.position = "top") +
  labs(
    title = paste0(election_of_interest$cycle," ",election_of_interest$state,election_of_interest$district," ",str_to_title(election_of_interest$office_full)," Race Receipts by Type"),
    subtitle = paste0("Period ending ",period_ending),
    x = "",
    y = "",
    caption = paste0("Source: Federal Election Commmission, data retreived on ",Sys.Date(), " by ",author),
    fill = "Party"
  ) +
  #geom_text(aes(x = last_name, y = percent, label = scales::percent(percent)),position = position_stack(vjust = .5)) +
  scale_y_continuous(labels = scales::dollar, limits = c(0,max(detailed_financial_df$total_receipts)*1.2)) +
  scale_fill_manual(values = colors) +
  coord_flip() +
  chart_theme

filename <- paste0(Sys.Date(),"-receipts-by-type-",election_of_interest$cycle,"-",election_of_interest$state,"-",election_of_interest$office_full,"-",election_of_interest$district,".png")
ggsave(filename = filename, plot = p4, path = output_path, width = 10, height = 6, unit = "in")

