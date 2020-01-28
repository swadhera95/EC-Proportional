rm(list=ls())
library(dplyr) # For Processing 
library(purrr) # For Processing
library(tibble) # For Processing
library(electoral) # For calculating D'Hondt Winner
library(ggplot2) # For Graphing  

## Calculating Electoral College Winner Using the D'Hondt Method
## Step 1: Read in Datasets
  # Vote Winners by State for 1976-2016
master <- read.csv(file="/Users/sidhantwadhera/Desktop/Other/EC D'Hondt/1976-2016-President.csv", 
                   header = T, stringsAsFactors = F)

  # Total Electoral College Votes Per State Per Year
EVs <- read.csv(file="/Users/sidhantwadhera/Desktop/Other/EC D'Hondt/EV Master.csv", 
                header = T, stringsAsFactors = F)

  # Actual Election Results 
real <- read.csv(file="/Users/sidhantwadhera/Desktop/Other/EC D'Hondt/EV_Real.csv", 
                 header = T, stringsAsFactors = F)

# Step 2: Data Processing 

master2 <- left_join(master, EVs, by=c("state_po")) # Merging Total state EVs to Master

master2$candidate <- ifelse(master2$candidate == "", "WriteIn", master2$candidate) # Make missing non-missing 
master2$party <- ifelse(master2$party == "", "WriteIn", master2$party)

# Fixing Issue in 2012 Election where Mitt Romney is entered twice 

master2$candidate <- ifelse(master2$candidate == "Mitt, Romney", "Romney, Mitt", master2$candidate)

# NOTE: There are cases where there are multiple Write In Candidates. Data Must Be Flattened for those cases

flat <- master2 %>%
  group_by(year, state, state_po, office, candidate) %>%
  summarise(candidatevotes = sum(candidatevotes), 
            totalvotes = max(totalvotes), 
            EV_1976 = max(EV_1976), 
            EV_1980 = max(EV_1980), 
            EV_1984 = max(EV_1984), 
            EV_1988 = max(EV_1988), 
            EV_1992 = max(EV_1992), 
            EV_1996 = max(EV_1996), 
            EV_2000 = max(EV_2000), 
            EV_2004 = max(EV_2004), 
            EV_2008 = max(EV_2008), 
            EV_2012 = max(EV_2012), 
            EV_2016 = max(EV_2016)) %>% 
  ungroup() 
  
# Candidate Vote Total in a Year
pv_c <- flat %>%
  group_by(year, candidate) %>%
  summarise(c_sum = sum(candidatevotes)) %>%
  ungroup 

# State Vote Total in a Year
pv_s <- flat %>%
  group_by(year, state) %>%
  summarise(total = max(totalvotes))

# Total Votes in a Year
pv_y <- pv_s %>%
  group_by(year) %>%
  summarise(t_sum = sum(total))

# Create Popular Vote Percent
pv <- pv_c %>%
  left_join(pv_y, by="year") %>%
  mutate(pv = (c_sum/t_sum)*100)

  # Function to Filter File by State

bystate <- function(x, df){
  state <- df %>%
    filter(state_po == x)
  
  return(state)
}

  # Function to use D'Hondt Method to Calculate EC Votes by State
seat_calc <- function(x, way){
  seats <- as.data.frame(seats_ha(x$candidate, x$candidatevotes, max(x[,7]), way))
  names(seats)[1] <- x$state_po
  seats$candidate <- rownames(seats)
  return(seats)
}

proportional_winner <- function(EY, way){
  
  ## Part A: Subset the Dataframe by Election Year
  
  Y <- flat %>%
    filter(year==EY) %>% 
    select(year, state, state_po, candidate, 
           candidatevotes, totalvotes, paste0("EV_",EY))
  
  ## Part B: Subset the Year Only DF by State
    
  states <- lapply(unique(Y$state_po), bystate, df = Y)
  
  ## Get D'Hondt Winner by State
  
  results <- lapply(states, seat_calc, way)
  
  ## Create Single Dataframe with rows for candidates and columns for states
  
  uniform <- results %>% reduce(full_join, by = "candidate") 
  
  ## Change Rowname to Candidate 
  
  row.names(uniform) <- uniform$candidate
  
  ## Drop Candidate Column 
  
  uniform$candidate <- NULL
  
  ## Get Row Sums for Each Candidate 
  
  uniform$total <- rowSums(uniform, na.rm = T)
  
  ## Clean Dataset with only candidates who received votes
  
  final <- uniform %>%
    rownames_to_column(var="candidate") %>%
    filter(uniform$total >= 1) %>%
    select(candidate, total) %>%
    mutate(year = EY) %>%
    arrange(total)
  
  ## Output Final Dataset 
  
  return(final)
}
# Get Results for Each Year
Results_1976 <- proportional_winner(1976, "dhondt")

Results_1980 <- proportional_winner(1980, "dhondt")

Results_1984 <- proportional_winner(1984, "dhondt")

Results_1988 <- proportional_winner(1988, "dhondt")

Results_1992 <- proportional_winner(1992, "dhondt")

Results_1996 <- proportional_winner(1996, "dhondt")

Results_2000 <- proportional_winner(2000, "dhondt")

Results_2004 <- proportional_winner(2004, "dhondt")

Results_2008 <- proportional_winner(2008, "dhondt")

Results_2012 <- proportional_winner(2012, "dhondt")

Results_2016 <- proportional_winner(2016, "dhondt")

# Bind together results
Results <- rbind.data.frame(Results_1976, Results_1980, Results_1984, 
                            Results_1988, Results_1992, Results_1996, 
                            Results_2000, Results_2004, Results_2008, 
                            Results_2012, Results_2016)

# Left Join Popular Vote and Real Results
  # Create Real Winner Flag
  # Create Electoral Vote Difference
  # Create Real Results Percent
  # Create Proportional Results Percent
  # Create Differences for Proportional and Real compared to Popular Vote
  # Drop weird NJ case where all non-major party candidates are entered as "writein"

Results2 <- Results %>%
  left_join(pv, by=c("candidate", "year")) %>%
  left_join(real, by=c("candidate", "year")) %>%
  mutate(winner = ifelse(EV_Total >= 270, 1, 0), 
         pv = round(pv, digits = 2), 
         Prop_EV = total, 
         Real_EV = EV_Total, 
         Prop_Pct = (total/538)*100, 
         Real_Pct = (EV_Total/538)*100, 
         Diff = (EV_Total - total), 
         RealPct_Diff = (Real_Pct - pv), 
         PropPct_Diff = (Prop_Pct - pv)) %>%
  select(candidate, year, winner, pv, Real_EV, Real_Pct, Prop_EV, Prop_Pct, Diff, RealPct_Diff, PropPct_Diff) %>%
  filter(candidate != "WriteIn")


R_1984 <- as.data.frame(t(Results2[Results2$year == 1984, c(1,4:8)]))
names(R_1984)[1] <- "Mondale, Walter"
names(R_1984)[2] <- "Reagan, Ronald"

row.names(R_1984) <- c("Candidate", "Popular Vote (%)", "Majoritarian Electoral Vote", "Maj. Electoral Vote (%)", "Proportional Electoral Vote", "Prop. Electoral Vote (%)")

R_1984 <- R_1984[-1, ]

# Mean Winner Overperformance: 
t.test(Results2[Results2$winner == 1, 9], 
       alternative = c("greater"), 
       mu = 0)

## Avg = 87.9, p-value is 0.001 with 10 DF

# Mean Winner Percent Overperformance
t.test(Results2[Results2$winner==1, 10], 
       alternative = c("greater"), 
       mu = 0)

## Avg = 18.1, p-value is 0.0006 with 10 df

# Mean Winner Prop Percent Over Performance 
t.test(Results2[Results2$winner == 1, 11],
       alternative = c("greater"), 
       mu = 0)

## Avg = 1.7, p-value is 0.0002 with 10 df

# Scatter of EC v. Popular Vote Percent

ggplot(Results2, aes(x=pv, y=Prop_EV)) +
  geom_point() +
  geom_smooth(method="lm", se=F) + 
  theme(axis.line.x = element_line(), 
        axis.line.y = element_line(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank()) +
  ggtitle("Relationship between Popular Vote Share (%) and Proportional Electoral Votes") + 
  xlab("Popular Vote Share (%)") + 
  ylab("Electoral Votes Under Proportional")

PEV_pv <- lm(Prop_EV ~ pv, Results2)
summary(PEV_pv)

cor(Results2$pv, Results2$Prop_EV)

## Scatter of Actual EC v. Popular Vote Percent 
ggplot(Results2, aes(x=pv, y=Real_EV)) +
  geom_point() +
  geom_smooth(method="loess") + 
  theme(axis.line.x = element_line(), 
        axis.line.y = element_line(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank()) +
  ggtitle("Relationship between Popular Vote Share (%) and Actual Electoral Votes") + 
  xlab("Popular Vote Share (%)") + 
  ylab("Electoral Votes Under Winner-Take-All")

REV_pv <- lm(Real_EV ~ pv, Results2)
summary(REV_pv)

cor(Results2$pv, Results2$Real_EV)








