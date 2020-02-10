
#### Good GSS cleaning file
gss08 <- read_dta("~/Dropbox/data/gss_data/gsspanels/gsspanel08.dta")


#convert the GSS from wide to long format
gss.long08 <- gss08 %>% select(-vetyear2) %>%
  mutate(idnum = id_1) %>%
  gather(v, value, abany_1:letin1a_3) %>% 
  separate(v, c("var", "wave")) %>% 
  mutate(var = tolower(var)) %>%
  arrange(idnum) %>% 
  spread(var, value) %>%
  mutate(idnum = paste("08-", idnum, sep = "")) 

#recode some variables:
gss.long08 <- gss.long08 %>%
  dplyr::mutate(
    #recode variables where "stay the same" is 3, making that 2
    finalter = recode(finalter, "1"=1, "2"=3, "3"=2),
    courts = recode(courts, "1"=1, "2"=3, "3"=2),
    divlaw = recode(divlaw, "1"=1, "2"=3, "3"=2),
    trust = recode(trust, "1"=1, "2"=3, "3"=2),
    fair = recode(fair, "1"=1, "2"=3, "3"=2),
    helpful = recode(helpful, "1"=1, "2"=3, "3"=2),
    aged = recode(aged, "1"=1, "2"=3, "3"=2),
    fear = ifelse(fear == 2, 0, fear),
    #Reliten as coded goes "strong", "not very strong" and "somewhat strong"
    #so recoded to more logical "strong", "somewhat strong", "not very strong"
    reliten = recode(reliten, "1"=1, "2"=3, "3"=2, "4"=4),
    #recode some reported values as missing
    racopen = ifelse(racopen == 3, NA, racopen),
    partyid = ifelse(partyid == 7, 3, partyid),
    bible = ifelse(bible == 4, NA, bible),
    #create some dummies from multinomial variables
    neverrelact = ifelse(relactiv == 1, 1, 0),
    relactiv = ifelse(relactiv == 1, 0, relactiv - 1),
    suicide1 = ifelse(suicide1 == 2, 0, 1),
    suicide2 = ifelse(suicide2 == 2, 0, 1),
    suicide3 = ifelse(suicide3 == 2, 0, 1),
    suicide4 = ifelse(suicide4 == 2, 0, 1),
    socbar = 8-socbar,
    socommun = 8 - socommun,
    socrel = 8 - socrel,
    socfrend = 8 - socfrend,
    xmovie = ifelse(xmovie == 1, 1, 0),
    news = 5 - news,
    owngun = ifelse(owngun == 3, NA, 
                    ifelse(owngun == 2, 1, 0)),
    polhitok = ifelse(polhitok == 2, 0, 1), 
    polmurdr = ifelse(polmurdr == 2, 0, 1), 
    polescap = ifelse(polescap == 2, 0, 1), 
    polabuse = ifelse(polabuse == 2, 0, 1), 
    polattak = ifelse(polattak == 2, 0, 1),
    selfhunt = ifelse(is.na(hunt), NA, ifelse(hunt %in% c(1, 3), 1, 0)),
    spkhomo = ifelse(spkhomo == 1, 1, 0),
    colhomo = ifelse(colhomo == 4, 1, 0),
    libhomo = ifelse(libhomo == 2, 1, 0),
    homoscale = spkhomo + colhomo + libhomo,
    spkath = ifelse(spkath == 1, 1, 0),
    colath = ifelse(colath == 4, 1, 0),
    libath = ifelse(libath == 2, 1, 0),
    athscale = spkath + colath + libath,
    spkrac = ifelse(spkrac == 1, 1, 0),
    colrac = ifelse(colrac == 4, 1, 0),
    librac = ifelse(librac == 2, 1, 0),
    racscale = spkrac + colrac + librac,
    spkmil = ifelse(spkmil == 1, 1, 0),
    colmil = ifelse(colmil == 4, 1, 0),
    libmil = ifelse(libmil == 2, 1, 0),
    milscale = spkmil + colmil + libmil,
    spkcom = ifelse(spkcom == 1, 1, 0),
    colcom = ifelse(colcom == 5, 1, 0),
    libcom = ifelse(libcom == 2, 1, 0),
    comscale = spkcom + colcom + libcom,
    fepol = ifelse(fepol == 2, 1, 0),
    abany = ifelse(abany == 1, 1, 0),
    abrape = ifelse(abrape == 1, 1, 0),
    abpoor = ifelse(abpoor == 1, 1, 0),
    abnomore = ifelse(abnomore == 1, 1, 0),
    absingle = ifelse(absingle == 1, 1, 0),
    abdefect = ifelse(abdefect == 1, 1, 0),
    abhlth = ifelse(abhlth == 1, 1, 0),
    kidssol = ifelse(kidssol == 6, NA, kidssol),
    chldidel = ifelse(chldidel == 8, NA, chldidel),
    cappun = ifelse(cappun == 1, 1, 0),
    grass = ifelse(grass == 1, 1, 0),
    gunlaw = ifelse(gunlaw == 1, 1, 0),
    letdie1 = ifelse(letdie1 == 1, 1, 0),
    postlife = ifelse(postlife == 1, 1, 0),
    prayer = ifelse(prayer == 1, 1, 0),
    racdif1 = ifelse(racdif1 == 1, 1, 0),
    racdif2 = ifelse(racdif2 == 1, 1, 0),
    racdif3 = ifelse(racdif3 == 1, 1, 0),
    racdif4 = ifelse(racdif4 == 1, 1, 0),
    racopen = ifelse(racopen == 1, 1, 0),
    richwork = ifelse(richwork == 1, 1, 0),
    sexeduc = ifelse(sexeduc == 1, 1, 0),
    uswary = ifelse(uswary == 1, 1, 0),
    reborn = ifelse(reborn == 1, 1, 0),
    relexp = ifelse(relexp == 1, 1, 0),
    relexper = ifelse(relexper == 1, 1, 0)) %>%
  #spending priority variables have to wordings,
  #collapse them
  mutate(natspac = ifelse(is.na(natspac), natspacy, natspac),
         natenvir = ifelse(is.na(natenvir), natenviy, natenvir),
         natheal = ifelse(is.na(natheal), nathealy, natheal),
         natcity = ifelse(is.na(natcity), natcityy, natcity),
         natcrime = ifelse(is.na(natcrime), natcrimy, natcrime),
         natdrug = ifelse(is.na(natdrug), natdrugy, natdrug),
         nateduc = ifelse(is.na(nateduc), nateducy, nateduc),
         natrace = ifelse(is.na(natrace), natracey, natrace),
         natarms = ifelse(is.na(natarms), natarmsy, natarms),
         nataid = ifelse(is.na(nataid), nataidy, nataid),
         natfare = ifelse(is.na(natfare), natfarey, natfare),
         partyid = ifelse(partyid == 7, 3, partyid),
         conmedia = contv + conpress) %>%
  #select "minimum" age for each person - their lowest age in the dataset (age at first interview)
  dplyr::group_by(idnum) %>%
  dplyr::mutate(minage = min(age, na.rm = TRUE),
                ds = "gss08") %>%
  ungroup()

alpha <- gss.long08 %>%
  select(abdefect, abhlth, abnomore, abpoor, abrape, absingle)
gss.long08$abscale6 <- psych::alpha(alpha)$scores
alpha <- gss.long08 %>%
  select(abdefect, abhlth, abnomore, abpoor, abrape, absingle, abany)
gss.long08$abscale7 <- psych::alpha(alpha)$scores
alpha <- gss.long08 %>%
  select(fepol, fechld, fepresch, fefam) %>%
  mutate(fepol = (fepol - mean(fepol, na.rm = TRUE))/sd(fepol, na.rm = TRUE),
         fechld = (fechld - mean(fechld, na.rm = TRUE))/sd(fechld, na.rm = TRUE),
         fepresch = (fepresch - mean(fepresch, na.rm = TRUE))/sd(fepresch, na.rm = TRUE),
         fefam = (fefam - mean(fefam, na.rm = TRUE))/sd(fefam, na.rm = TRUE))
gss.long08$genderscale <- psych::alpha(alpha, check.keys = TRUE)$scores
alpha <- gss.long08 %>%
  select(trust, helpful, fair) %>%
  mutate(fair = 4 - fair)
gss.long08$misanthropy <- psych::alpha(alpha)$scores
alpha <- gss.long08 %>%
  select(socbar, socrel, socfrend, socommun) 
gss.long08$socialscale <- psych::alpha(alpha)$scores
alpha <- gss.long08 %>%
  select(suicide1, suicide2, suicide3, suicide4)
gss.long08$suicidescale <- psych::alpha(alpha)$scores
alpha <- gss.long08 %>%
  select(polhitok, polabuse, polescap, polmurdr, polattak)
gss.long08$polviolent <- psych::alpha(alpha)$scores



gss.long08 <- gss.long08 %>%
  select(idnum, wave, ds, minage,
         #religious activity (4)
         attend, relactiv, pray, neverrelact,
         #religious beliefs (11)
         postlife, bible, relpersn, sprtprsn, god, reliten, reborn, relexp, 
         relexper, fund, popespks, 
         #social life (8)
         socbar, socommun, socrel, socfrend, news, tvhours, xmovie, socialscale, 
         #subjective SES (9)
         finalter, finrela, jobfind, kidssol, goodlife, joblose, parsol, 
         class, getahead, 
         #Suicide (6)
         suicide1, letdie1, suicide4, suicide2, suicidescale, suicide3, 
         #Guns, etc. (14)
         owngun, selfhunt, polhitok, polattak, polescap, polabuse, polmurdr, 
         polviolent, divlaw, fear, gunlaw, grass, courts, cappun,  
         #Political Views (9)
         partyid, polviews, helppoor, prayer, helpsick, uswary, helpblk, helpnot,
         eqwlth, 
         #Spending priorities (17)
         natarms, natchld, natcity, natcrime, natdrug, nateduc, 
         natenvir, natfare, natheal, natmass, natpark, natrace, 
         natroad, natsci, natsoc, natspac, nataid, 
         #Civil liberties (20)
         spkcom, spkath, colhomo, libhomo, spkhomo, colath, libcom, colcom, spkmil, 
         libath, colmil, spkrac, librac, colrac, libmil,
         homoscale, comscale, athscale, milscale, racscale, 
         #Confidence in institutional leaders (14)
         conclerg, conlabor, conarmy, conjudge, coneduc, conlegis, consci, 
         conpress, conmedic, conbus, confinan, confed, contv, conmedia, 
         #Health and Morale (7)
         satjob, satfin, happy, life, health, richwork, hapmar, 
         #Social trust (4)
         trust, fair, helpful, misanthropy, 
         #Family (18)
         obey, thnkself, popular, helpoth, chldidel, workhard,   
         fepol, fejobaff, fefam, fepresch, spanking, genderscale,
         fechld, aged, fehire, meovrwrk, discaffm, discaffw, 
         #Race and Immigration (21)
         racdif2, racdif4, racdif1, racdif3, 
         marwht, marblk, workblks, workwhts, intlwhts, intlblks, 
         livewhts, liveblks, wlthblks, wlthwhts, racopen, discaff,  
         closeblk, closewht, affrmact, letin1a, wrkwayup, 
         #Sex, sexuality and abortion (17)
         abdefect, absingle, abrape, abpoor, abany, abhlth, abnomore, abscale6,
         abscale7, homosex, premarsx, teensex, xmarsex, pillok, pornlaw,  
         marhomo, sexeduc)

gss.wide08 <- gss.long08 %>%
  filter(!is.na(wave)) %>%
  #gather all variables observed in multiple waves to one key, value pair of columns
  #inspect your data, make sure that this third term captures any variables you
  #might have created. "sexeduc" is just the last 
  gather(v, value, attend:sexeduc) %>%
  #combine variable name and wave columns to get "abany_1" syntax
  unite(v, c("v", "wave")) %>%
  #spread new variables over columns
  spread(v, value)


