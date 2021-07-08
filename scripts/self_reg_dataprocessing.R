## 
## Here we do data processing and editing 

# note: we need to determine which dataset best to use.
# for now we are going with the standford talk version, which is certainly sufficient to get us started. 

# DF = read_excel("DF_StanfordTalk_Master_20200221.xlsx")
source("scripts/self_reg_functions.r") # the functions in functions are available
DF = read_excel("data/DF_StanfordTalk_Master_20200221.xlsx")
DF = miss2NA3(DF, gmc) # all missing data codes are NA

loc_key = c(MA = "India", SP = "Ghana", TA = "Vanuatu", MM = "Malaysia",EC='Austin') # to rename locations with normal names

dropvec = c('notes','year','month','day','time','task','filmed','comments')
DFs = DF %>% 
  mutate(age_yr = if_else(location=='TA',age_pid,
                          if_else(location == 'MM', age_pid,
                                  if_else(location == 'MA',age_pid,
                                          if_else(location == 'EC',age.BIO,
                                                  if_else(location=='SP',age.PIDr,'NA'))))),
         age_yr = as.numeric(age_yr),
         #sex = if_else(location %in% c('TA','MM','MA','MA'), sex.PIDr,sex.BIO)) %>% 
         sex = case_when(location %in% c('TA','MM','MA','MA','SP') ~ sex.PIDr, 
                         location %in% c('EC') ~ sex.BIO)) %>%
  filter(age_yr <= 13 & age_yr >=4) %>%
  select(-contains(dropvec))


# later on we will remove the necklace data prep, which is here unneccessarily at the moment. 

# this cleans up several bead columns for bead task
# this applies to several columns, ok, here we go... 
DFs = DFs %>%
  mutate(num_string_stretch = coalesce(num_string_stretch,blu_num_string_stretch, rd_num_string_stretch),
         string_flat = coalesce(string_flat, blu_string_flat, rd_string_flat),
         beads_used_YS = coalesce(beads_used_YS, blu_beads_used_YS, rd_beads_used_YS),
         beads_used_BS = coalesce(beads_used_BS, blu_beads_used_BS, rd_beads_used_BS),
         beads_used_BS = coalesce(beads_used_RS, blu_beads_used_RS, rd_beads_used_RS),
         beads_used_PC = coalesce(beads_used_PC, blu_beads_used_PC, rd_beads_used_PC),
         beads_used_GC = coalesce(beads_used_GC, blu_beads_used_GC, rd_beads_used_GC),
         beads_used_OC = coalesce(beads_used_OC, blu_beads_used_OC, rd_beads_used_OC),
         final_seq = coalesce(final_seq, blu_final_seq, rd_final_seq))

# the necklace data is difficult to prepare 
# revisiting this code later on, we don't remember why we did any of this 
# prepare necklace vector
neckcols = c('p','g','o','b','r','y')
neckshapes = c('c','s')
colmatch = str_c(neckcols,collapse = '|')
shapematch = str_c(neckshapes, collapse = '|')
xx = str_replace_all(DFs$final_seq, "[[:punct:]]", "")
xx2 = str_replace_all(xx, " ","")
xx3 = str_extract_all(xx2,colmatch,simplify = TRUE)
xx4 = str_extract_all(xx2,shapematch,simplify = TRUE)
DFs$neckcolvec = apply(xx3, 1, paste, collapse = "")
DFs$neckshapevec = apply(xx4, 1, paste, collapse = "")
DFs$neckcolmatch = str_detect(DFs$neckcolvec,'pyg')
DFs$neckshapematch = str_detect(DFs$neckshapevec,'csc')
# to check: 
neckcheck= DFs %>% select(location,final_seq,neckcolvec,neckcolmatch,neckshapevec,neckshapematch)# %>% View() #%>% filter(!is.na(final_seq)) #%>% View()
#madenecks = which(!is.na(DFsm$final_seq))
#DFs$neck_made = 

# we also have to fix the necklace stuff 
ec_neck = DFs %>% select(1:3,93:120,124:134) %>% filter(location=='EC')
# issue 1 is that we have final seq broken out into two rows. 

# write.csv(neckcheck,'necklacecodingquickcheck.csv')

#   DFs = DFs %>% 
# mutate(neckseq = str_replace_all(final_seq, "[[:punct:]]", ""),
#        neckseq = str_replace_all(neckseq, " ","")) %>% 
# mutate(xx = str_extract_all(neckseq,colmatch,simplify = TRUE))
#       colors = apply(str_extract_all(neckseq,colmatch),1,paste,collapse = ""))
DFsm = DFs %>% 
  mutate(yn_mmt = if_else(treat_eat == 'y',1, if_else(treat_eat == 'n',0,NaN))) %>%
  mutate(
    #sex = as_factor(sex.PIDr), 
    weight = as_numeric(weight), 
    height2 = as_numeric(coalesce(height,height_stand)),
    math_total = as_numeric(math_total),
    reading_total = as_numeric(reading_total),
    part_i = as_numeric(part_i),
    part_ii = as_numeric(part_ii),
    part_iii = as_numeric(part_iii),
    HTKS = as_numeric(part_i+part_ii+part_iii),
    yn_hookedit = as_numeric(str_replace_all(success, c('y'= '1','n'='0'))),
    method_andfail = case_when(method == '1' ~ 'hook', 
                               method == '2' ~ 'drag',
                               method == '3' ~ 'other',
                               yn_hookedit == 0 ~ 'fail'),
    location = recode_factor(location, !!!loc_key),
    location = as_factor(location)) %>%
  mutate_at(., c('age_pid','age_adint','age.PIDr','age.BIO', 'height_stand', 'weight','num_tp_box_taps',
                 'num_other_box_taps','num_string_stretch','beads_used_YS','beads_used_BS','beads_used_RS',
                 'beads_used_PC','beads_used_GC','beads_used_OC'),as_numeric) %>% 
  mutate_at(.vars = c('X_traced_1','X_traced_2','box_rotate_45','box_opened','used_stick',
                      'reward_rtrvd','stick_reward','mem_Q1_rot','mem_Q2_tap','mem_Q3_swipe',
                      'mem_Q4_trace','string_flat','beads_frhd_YS','beads_frhd_BS',
                      'beads_frhd_RS','beads_frhd_PC','beads_frhd_GC','beads_frhd_OC',
                      'PC_to_head','OC_to_cheek','redstr_to_head','blue_str_flat'), funs(as_numeric(str_replace_all(., c('y'= '1','n'='0'))))) %>%
  mutate_at(.vars = c('X_traced_1','X_traced_2','box_rotate_45','box_opened','used_stick',
                      'reward_rtrvd','stick_reward','mem_Q1_rot','mem_Q2_tap','mem_Q3_swipe',
                      'mem_Q4_trace','string_flat','beads_frhd_YS','beads_frhd_BS',
                      'beads_frhd_RS','beads_frhd_PC','beads_frhd_GC','beads_frhd_OC',
                      'PC_to_head','OC_to_cheek','redstr_to_head','blue_str_flat','num_tp_box_taps',
                      'num_other_box_taps','num_string_stretch','beads_used_YS','beads_used_BS','beads_used_RS',
                      'beads_used_PC','beads_used_GC','beads_used_OC'), funs(replace(., is.na(.),0))) %>%
  mutate(puzzlebox_quotient = (num_tp_box_taps +  box_rotate_45 + X_traced_1 + X_traced_2 + used_stick + stick_reward)/
           (num_tp_box_taps +  X_traced_1 + X_traced_2 + box_rotate_45 + used_stick + stick_reward + box_opened + reward_rtrvd), # puzz ovim quotient 
         beads_frhd_total = (beads_frhd_YS + beads_frhd_BS + beads_frhd_RS + beads_frhd_PC + 
                               beads_frhd_GC + beads_frhd_OC),
         beads_used_total = (beads_used_YS + beads_used_BS + beads_used_RS +
                               beads_used_PC + beads_used_GC + beads_used_OC),
         necklace_quotient = (num_string_stretch + string_flat + beads_frhd_total)/ (num_string_stretch + string_flat + beads_frhd_total + beads_used_total), # necklace quotient
         puzz_if_tpbox = if_else(num_tp_box_taps ==3, 2, if_else(num_tp_box_taps == 0,0,1)), # puzz Imitative Fidelity starts here
         puzz_if_x = X_traced_1 + X_traced_2, 
         puzz_if_rest = 2*(used_stick+box_rotate_45+stick_reward+box_opened+reward_rtrvd),
         puzz_if_total = puzz_if_tpbox + puzz_if_x + puzz_if_rest,
         neck_if_beadsfh = if_else(beads_frhd_total ==3, 2, if_else(beads_frhd_total == 0,0,1)), # necklace imitative fidelity starts here 
         neck_if_ststch = if_else(num_string_stretch ==2, 2, if_else(num_string_stretch == 0,0,1)),
         neck_if_beadsuse = if_else(beads_used_total ==3, 2, if_else(beads_used_total == 0,0,1)),
         neck_if_strflat = 2*(string_flat),
         neck_if_strcol = if_else(string_used == 'red',2, if_else(string_used %in% c('blue','green'),1,0)), # 
         #neck_if_colseq = if_else(neckcolmatch == TRUE, 2, if_else(is.na(neckcolvec), 0,1)),
         neck_if_colseq = case_when(neckcolmatch == TRUE ~ 2,
                                    is.na(final_seq) ~ 0, 
                                    !is.na(final_seq) ~ 1),
         #neck_if_shapeseq = if_else(neckshapematch == TRUE, 2, if_else(!is.na(neckshapevec), 1,0)),
         neck_if_shapeseq = case_when(neckshapematch == TRUE ~ 2,
                                      is.na(final_seq) ~ 0, 
                                      !is.na(final_seq) ~ 1)) %>%
  mutate_at(.vars= c("neck_if_beadsfh","neck_if_ststch","neck_if_beadsuse","neck_if_strflat","neck_if_strcol",
                     "neck_if_colseq", "neck_if_shapeseq"), funs(replace(., is.na(.),0))) %>%
  mutate(neck_if_total = neck_if_beadsfh+neck_if_ststch+neck_if_beadsuse+neck_if_strflat+neck_if_strcol+neck_if_colseq+neck_if_shapeseq)
# check
#DFsm %>% select(final_seq, starts_with('neck')) %>% View()
DFsm$age_yr2 = round(DFsm$age_yr,0)
DFsm$ageclass<-cut(DFsm$age_yr2,c(0,7,10,13),right=FALSE,labels = c('4-6','7-9','10-12'))

#table(DFsm$ageclass,DFsm$age_yr2)
# another check. lets show the necklace fidelity coding to Bruce
#randrows = as.integer(runif(30,1,length(DFsm$PID)))
#neckcheck = DFsm %>% select(PID_location, final_seq, beads_frhd_total, num_string_stretch, beads_used_total, string_flat, string_used, neckcolvec, neckcolmatch, neckshapevec, #neckshapematch, starts_with('neck_')) %>% slice(randrows)
#write.csv(neckcheck,'BrucePleasecheckthis.csv')

# creating centered and scaled vars
DFsm = DFsm %>%
  group_by(location)%>%
  mutate(reading_cent = scale(reading_total, center=TRUE, scale = FALSE),
         math_cent = scale(math_total, center=TRUE, scale = FALSE),
         reading_z = scale(reading_total, center=TRUE, scale = TRUE),
         math_z = scale(math_total, center=TRUE, scale = TRUE),
         age_z = scale(age_yr2, center=TRUE, scale = TRUE)) %>% 
  ungroup()

# replace a zero score in HTKS with NA 
# DFsm$HTKS = na_if(DFsm$HTKS,0) # EM commented this out as 0 indicates kids had really low cognitive load

# AKA reformat 
df_aka = DFsm %>% select(PID,location, sex, age_yr, reading_total, math_total, reading_cent, math_cent) #%>%
#  group_by(location) %>% 
#  mutate(reading_cent = scale(reading_total, center=TRUE, scale = FALSE),
#         math_cent = scale(math_total, center=TRUE, scale = FALSE)) %>%
#  ungroup()

df_aka_c = pivot_longer(df_aka, cols = -c(PID,location,sex,age_yr),
                        names_to = c('test'),
                        values_to = c('score'))

test_key = c(reading_total = "Reading", math_total = "Math", reading_cent = 'Reading_c',math_cent = 'Math_c')
df_aka_c = df_aka_c %>% mutate(test = recode_factor(test, !!!test_key))

# oh gosh, what have we done here? 