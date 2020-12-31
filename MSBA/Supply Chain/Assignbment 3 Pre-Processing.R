library(fpp)
library(reshape)
library(dplyr)
library(glmnet)

#
PBS <- read.csv("Peanut Butter Chicago.csv")[,-1] %>% 
  mutate( F_LSA=ifelse(F=="A",1,0),   # Large Size Ad Dummy
          F_MSA=ifelse(F=="B",1,0),   # Medium Size Ad Dummy
          F_SSA=ifelse(F=="C",1,0),   # Small Size Ad Dummy
          D_MIN=ifelse(D==1,1,0),     # Minor In-Store Display Dummy
          D_MAJ=ifelse(D==2,1,0)) %>% # Major In-Store Display Dummy
  # Promotional variables are weighted by sales volume (oz)
  mutate(S_LB = UNITS * VOL_EQ,
         WF_LSA = F_LSA * UNITS * VOL_EQ,     # Large Size Ad Weighted
         WF_MSA = F_MSA * UNITS * VOL_EQ,     # Medium Size Ad Weighted
         WF_SSA = F_SSA * UNITS * VOL_EQ,     # Small Size Ad Weighted
         WD_MIN = D_MIN * UNITS * VOL_EQ,     # Minor In-Store Display Weighted
         WD_MAJ = D_MAJ * UNITS * VOL_EQ) %>% # Major In-Store Display Weighted
  mutate(VEND =ifelse(VEND == 48001,"SK",ifelse( VEND == 99998,"PL","OB"))) %>%
  select(-F, -D)

# Create aggregate variables by product-week
x.pw <- group_by(PBS, WEEK, VEND) %>% 
  summarise(S.DOLLARS = sum(DOLLARS),      # Total $ Sales 
            S.S_LB    = sum(S_LB),         # Total L. Sales
            S.WF_LSA  = sum(WF_LSA),       # Total Weighted Large Ad
            S.WF_MSA  = sum(WF_MSA),       # Total Weighted Medium Ad
            S.WF_SSA  = sum(WF_SSA),       # Total Weighted Small Ad
            S.WD_MIN  = sum(WD_MIN),       # Total Weighted Minor Store Disp
            S.WD_MAJ  = sum(WD_MAJ)) %>%   # Total Weighted Major Store Disp
  # Calculate weigted averages of Advertising and Promotion variables
  mutate(A.PPU = log(S.DOLLARS / S.S_LB),  # Avg. Price per unit (pound)
         S.WF_LSA  = S.WF_LSA / S.S_LB,    # Avg. Weighted Large Ad
         S.WF_MSA  = S.WF_MSA / S.S_LB,    # Avg. Weighted Medium Ad
         S.WF_SSA  = S.WF_SSA / S.S_LB,    # Avg. Weighted Small Ad
         S.WD_MIN  = S.WD_MIN / S.S_LB,    # Avg. Weighted Minor Store Disp
         S.WD_MAJ  = S.WD_MAJ / S.S_LB)    # Avg. Weighted Major Store Disp
#


xmat <- x.pw %>%
  mutate(LS  = log(S.S_LB)) %>% 
  select(-S.DOLLARS, -S.S_LB)
#
# Creeate separate columns for vars of each brand group
xmat <- data.frame(filter(xmat, VEND == "SK"),
                   filter(xmat, VEND == "OB"),
                   filter(xmat, VEND == "PL")) %>%
  select(-WEEK, -WEEK.1, -WEEK.2, 
         -VEND, -VEND.1, -VEND.2, 
         -LS.1, -LS.2) # After droping vars. you should have 19 vars left

#
xm <- model.matrix(LS ~., data=xmat)[,-1]
y <- xmat[,"LS"]
#
