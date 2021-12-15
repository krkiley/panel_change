
#### Good GSS cleaning file

gss10 <- read_dta("~/Dropbox/data/gss_data/gsspanels/gsspanel10.dta")

#convert the GSS from wide to long format
gss.long10 <- gss10 %>% 
  mutate(idnum = id_1) %>%
  gather(v, value, abany_1:letin1a_3) %>% 
  separate(v, c("var", "wave")) %>% 
  mutate(var = tolower(var)) %>%
  arrange(idnum) %>% 
  spread(var, value) %>%
  mutate(idnum = paste("10-", idnum, sep = "")) 

#recode some variables:
gss.long10 <- gss.long10 %>%
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
    
    spkmslm = ifelse(spkmslm == 1, 1, 0),
    colmslm = ifelse(colmslm == 4, 1, 0),
    libmslm = ifelse(libmslm == 2, 1, 0),
    mslmscale = spkmslm + colmslm + libmslm,
    
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
                ds = "gss10") %>%
  ungroup()

alpha <- gss.long10 %>%
  select(abdefect, abhlth, abnomore, abpoor, abrape, absingle)
gss.long10$abscale6 <- psych::alpha(alpha)$scores
alpha <- gss.long10 %>%
  select(abdefect, abhlth, abnomore, abpoor, abrape, absingle, abany)
gss.long10$abscale7 <- psych::alpha(alpha)$scores
alpha <- gss.long10 %>%
  select(fepol, fechld, fepresch, fefam) %>%
  mutate(fepol = (fepol - mean(fepol, na.rm = TRUE))/sd(fepol, na.rm = TRUE),
         fechld = (fechld - mean(fechld, na.rm = TRUE))/sd(fechld, na.rm = TRUE),
         fepresch = (fepresch - mean(fepresch, na.rm = TRUE))/sd(fepresch, na.rm = TRUE),
         fefam = (fefam - mean(fefam, na.rm = TRUE))/sd(fefam, na.rm = TRUE))
gss.long10$genderscale <- psych::alpha(alpha, check.keys = TRUE)$scores
alpha <- gss.long10 %>%
  select(trust, helpful, fair) %>%
  mutate(fair = 4 - fair)
gss.long10$misanthropy <- psych::alpha(alpha)$scores
alpha <- gss.long10 %>%
  select(socbar, socrel, socfrend, socommun) 
gss.long10$socialscale <- psych::alpha(alpha)$scores
alpha <- gss.long10 %>%
  select(suicide1, suicide2, suicide3, suicide4)
gss.long10$suicidescale <- psych::alpha(alpha)$scores
alpha <- gss.long10 %>%
  select(polhitok, polabuse, polescap, polmurdr, polattak)
gss.long10$polviolent <- psych::alpha(alpha)$scores



gss.long10 <- gss.long10 %>%
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
         libath, colmil, spkrac, librac, colrac, libmil, spkmslm, colmslm, libmslm,
         homoscale, comscale, athscale, milscale, racscale, mslmscale,
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

gss.wide10 <- gss.long10 %>%
  filter(!is.na(wave)) %>%
  #gather all variables observed in multiple waves to one key, value pair of columns
  #inspect your data, make sure that this third term captures any variables you
  #might have created. "Zodiac" is just the last in the original dataset
  gather(v, value, attend:sexeduc) %>%
  #combine variable name and wave columns to get "abany_1" syntax
  unite(v, c("v", "wave")) %>%
  #spread new variables over columns
  spread(v, value)


weight06 <- gss06 %>% mutate(idnum = paste0("06", "-", id_1)) %>% select(idnum, wtpannr12, wtpannr123) 
weight08 <- gss08 %>% mutate(idnum = paste0("08", "-", id_1)) %>% select(idnum, wtpannr12, wtpannr123)
weight10 <- gss10 %>% mutate(idnum = paste0("10", "-", id_1),
                             wtpannr123 = WTPANNR123) %>% select(idnum, wtpannr12, wtpannr123)
all.weights <- bind_rows(weight06, weight08, weight10)


gss.wide <- bind_rows(gss.wide06, gss.wide08, gss.wide10) %>% left_join(all.weights)
gss.long <- gss.wide %>% 
  gather(key = "variable", value = "value", -c(idnum, ds, minage, wtpannr12, wtpannr123)) %>%
  separate(variable, into = c("var", "wave")) %>%
  spread(var, value) %>%
  mutate(ds = recode(ds, "gss06"="06", "gss08"="08", "gss10"="10"))
  
save(gss.long, file = "~/Desktop/panel_change/data/gsslong.Rdata")

rm(gss.long06, gss.long08, gss.long10, gss.wide06, gss.wide08, gss.wide10, weight06, weight08,
   weight10, all.weights, gss06, gss08, gss10)


#create a data frame of the variables and how many levels they have
#and give them small and large category labels
attitude_vars <- data.frame(var = names(gss.long), 
                            levels = apply(gss.long, 2, 
                                           function(x) length(unique(x[!is.na(x)])))) %>%
  remove_rownames() %>%
  filter(var %!in% c("idnum", "ds", "minage", "wtpannr123", "wave")) %>%
  mutate(small.cat = 
           ifelse(var %in% c("class", "parsol", "kidssol", "finrela", "getahead",
                             "finalter", "joblose", "jobfind", "goodlife"),
                  "Subjective SES", NA),
         small.cat = 
           ifelse(var %in% c("attend", "pray", "never", "relactiv", "neverrelact"), 
                  "Relig. Activity", small.cat),
         small.cat = 
           ifelse(var %in% c("postlife", "god", "bible", "reborn", "relexp", 
                             "relexper", "relpersn", "sprtprsn", "popespks", 
                             "none", "reliten", "fund"),
                  "Relig. identity & beliefs", small.cat),
         small.cat = 
           ifelse(var %in% c("letdie1", "suicide1", "suicide2", "suicide3", 
                             "suicide4","suicidescale"), 
                  "Suicide", small.cat),
         small.cat = 
           ifelse(var %in% c("socbar", "socialscale", "socommun", "socrel", 
                             "socfrend", "xmovie", "news", "tvhours"), 
                  "Social life", small.cat),
         small.cat = 
           ifelse(var %in% c("owngun", "selfhunt", "sphunt", "gunlaw", "cappun",
                             "fear", "grass", "divlaw", "courts", "polhitok",
                             "polmurdr", "polescap", "polabuse", "polattak",
                             "polviolent"),
                  "Guns, laws, crime & police", small.cat),
         small.cat = 
           ifelse(var %in% c("partyid", "polviews", "uswary", "prayer", "taxes",
                             "helpblk", "helpsick", "helppoor", "helpoth", 
                             "helpnot", "eqwlth"),
                  "Politics & government", small.cat),
         small.cat = 
           ifelse(var %in% c("natrace", "natspac", "natenvir", "natfare", 
                             "nateduc", "nataid", "natarms", "natsoc", "natchld", 
                             "natmass", "natcrime", "natheal", "natroad", 
                             "natsci", "natpark", "natdrug", "natcity"), 
                  "Public spending", small.cat),
         small.cat = 
           ifelse(var %in% c("spkhomo", "colhomo", "libhomo", "homoscale",
                             "spkcom", "colcom", "libcom", "comscale",
                             "spkath", "colath", "libath", "athscale",
                             "spkmil", "colmil", "libmil", "milscale",
                             "spkrac", "colrac", "librac", "racscale",
                             "spkmslm", "colmslm", "libmslm", "mslmscale"), 
                  "Civil liberties", small.cat),
         small.cat = 
           ifelse(var %in% c("trust", "fair", "helpful", "misanthropy"), 
                  "Trust", small.cat),
         small.cat = 
           ifelse(var %in% c("confed", "contv", "confinan", "conmedia", "conclerg",
                             "conpress", "conarmy", "conjudge", "consci", "conlabor",
                             "conmedic", "conbus", "coneduc", "conlegis"), 
                  "Confidence in leadership", small.cat),
         small.cat = 
           ifelse(var %in% c("health", "happy", "hapmar", "satfin", "life", 
                             "satjob", "richwork"), 
                  "Health & morale", small.cat),
         small.cat = 
           ifelse(var %in% c("abrape", "abpoor", "abnomore", "abhlth", "absingle",
                             "abdefect", "abany", "abscale6", "abscale7", 
                             "marhomo", "homosex", "premarsx", "xmarsex",
                             "teensex", "sexeduc", "pornlaw", "pillok"), 
                  "Sex, sexuality & abortion", small.cat),
         small.cat = 
           ifelse(var %in% c("aged", "spanking", "obey", "thnkself", "popular", 
                             "helpoth", "workhard", "fepol", "fefam", "fehire",
                             "fepresch", "fechld", "genderscale", "fejobaff", 
                             "meovrwrk", "discaffw", "discaffm", "chldidel"), 
                  "Gender & family", small.cat),
         small.cat = 
           ifelse(var %in% c("affrmact", "wrkwayup", "discaff", "racdif1", 
                             "racdif2", "racdif3", "racdif4", "closeblk", 
                             "marblk", "closewht", "marwht", "racopen", 
                             "liveblks", "livewhts", "workwhts", "workblks", 
                             "intlblks", "intlwhts", "wlthwhts", "wlthblks", 
                             "letin1a"), 
                  "Race & immigration", small.cat)) %>%
  mutate(var.name = 
           recode(var, "abany"="Abortion: any reason", 
                  "abdefect"="Abortion: birth defect",
                  "abhlth"="Abortion: mother's health", 
                  "abnomore"="Abortion: birth control",
                  "abpoor"="Abortion: too poor", 
                  "abrape"="Abortion: rape", 
                  "abscale6"="Abortion scale (6 items)", 
                  "abscale7"="Abortion scale (7 items)",
                  "absingle"="Abortion: unmarried",
                  "affrmact"="Hiring preferences",
                  "aged"="Seniors should live with family",
                  "athscale"="Athiest: scale", 
                  "attend"="Attend religious services",
                  "bible"="View of bible", 
                  "cappun"="Death penalty", 
                  "chldidel"="Ideal # of children",
                  "class"="Social class", 
                  "closeblk"="Feel close to blacks",
                  "closewht"="Feel close to whites",
                  "colath"="Athiest: Teach at college", 
                  "colcom"="Red: Teach at college", 
                  "colhomo"="Gay: Teach at college",
                  "colmil"="Militarist: Teach at college", 
                  "colrac"="Racist: Teach at college",
                  "colmslm"="Islamist: Teach at college",
                  "comscale"="Red: scale",
                  "conarmy"="Military", 
                  "conbus"="Major companies", 
                  "conclerg"="Organized religion", 
                  "coneduc"="Education",
                  "confed"="Executive branch", 
                  "confinan"="Banks & finance",
                  "conjudge"="Supreme Court", 
                  "conlabor"="Organized labor",
                  "conlegis"="Congress", 
                  "conmedia"="TV & press", 
                  "conmedic"="Medicine", 
                  "conpress"="Press",
                  "consci"="Scientific community", 
                  "contv"="Television",
                  "courts"="Courts deal with criminals",
                  "discaff"="Affirm. action hurt whites",
                  "discaffm"="Affirm. action hurt men",
                  "discaffw"="Affirm. action hurt women",
                  "divlaw"="Divorce laws too lenient", 
                  "eqwlth"="Government reduce inequality",
                  "fair"="People try to be fair", 
                  "fear"="Afraid to walk at night",
                  "fechld"="Working moms warm", 
                  "fefam"="Better for man to work", 
                  "fehire"="Hire & promote women", 
                  "fejobaff"="Preferences for hiring women",
                  "fepol"="Men better for politics",
                  "fepresch"="Pre-Ks suffer if mom works", 
                  "finalter"="Finances better or worse",
                  "finrela"="Relative income",
                  "fund"="Fundamentalist", 
                  "genderscale"="Scale of gender role attitudes",
                  "getahead"="Work to get ahead", 
                  "god"="Belief about God",
                  "goodlife"="Finances will improve",
                  "grass"="Legalize marijuana",
                  "gunlaw"="Require gun permits", 
                  "hapmar"="Happiness: marriage",
                  "happy"="Happiness: general", 
                  "health"="Health: self-rated",
                  "helpblk"="Government help blacks", 
                  "helpful"="People try to be helpful",
                  "helpnot"="Government do more or less",
                  "helpoth"="Child should help others",
                  "helppoor"="Government help poor",
                  "helpsick"="Government pay medical",
                  "homoscale"="Gay: scale",
                  "homosex"="Morality: Same-sex sex",
                  "intlblks"="Blacks intelligent or not",
                  "intlwhts"="Whites intelligent or not", 
                  "jobfind"="Find a good job", 
                  "joblose"="Likely to lose job",
                  "kidssol"="Mobility next generation",
                  "letdie1"="Let die: incurable",
                  "letin1a"="How many immigrants?", 
                  "libath"="Athiest: Library book", 
                  "libcom"="Red: Library book",
                  "libhomo"="Gay: Library book", 
                  "libmil"="Militarist: Library book",
                  "libmslm"="Islamist: Library book",
                  "librac"="Racist: Library book", 
                  "life"="Life: exciting or dull",
                  "liveblks"="Neighborhood half black",
                  "livewhts"="Neighborhood half white",
                  "marblk"="Relative marry black",
                  "marhomo"="Gay marriage",
                  "married"="Married now",
                  "marwht"="Relative marry white", 
                  "meovrwrk"="Men hurt family when overwork",
                  "milscale"="Militarist: scale", 
                  "misanthropy"="Misanthropy scale",
                  "mslmscale"="Islamist: Scale",
                  "nataid"="Foreign aid", 
                  "natarms"="Defense", 
                  "natchld"="Child care",
                  "natcity"="Problems of cities", 
                  "natcrime"="Fighting crime",
                  "natdrug"="Dealing with drugs", 
                  "nateduc"="Education",
                  "natenvir"="Protect environment", 
                  "natfare"="Welfare",
                  "natheal"="Improving health", 
                  "natmass"="Mass transit",
                  "natpark"="Parks & recreation", 
                  "natrace"="Improve life for blacks",
                  "natroad"="Highways & bridges", 
                  "natsci"="Scientific research",
                  "natsoc"="Social security", 
                  "natspac"="Space exploration",
                  "neverrelact"="Never active in religion",
                  "news"="Read newspaper",
                  "obey"="Child should obey",
                  "owngun"="Gun owner", 
                  "parsol"="Mobility",
                  "partyid"="Political Party", 
                  "pillok"="Birth control 14-16 year olds",
                  "polabuse"="Police hit person cursing", 
                  "polattak"="Police hit person punching",
                  "polescap"="Police hit person fleeing", 
                  "polhitok"="Police can hit citizens",
                  "polmurdr"="Police hit murder suspect", 
                  "polviews"="Political views",
                  "polviolent"="Police hit: scale", 
                  "popespks"="Pope infallible",
                  "popular"="Child should be popular", 
                  "pornlaw"="Restrict pornography",
                  "postlife"="Life after death", 
                  "pray"="Pray", 
                  "prayer"="Bible prayer in schools", 
                  "premarsx"="Morality: Premarital sex",
                  "racdif1"="Racial diff.: discrimination",
                  "racdif2"="Racial diff.: inborn", 
                  "racdif3"="Racial diff.: lack education",
                  "racdif4"="Racial diff.: lack will", 
                  "raclive"="Segregrated n'hood",
                  "racscale"="Racist: scale", 
                  "racopen"="Open housing law",
                  "racwork"="Segregated workplace", 
                  "reborn"="Born again",
                  "relactiv"="Do religious activities", 
                  "relexp"="Had religious experience",
                  "relexper"="Religious turning point", 
                  "reliten"="Strength of affiliation",
                  "relpersn"="Religious person",
                  "reltrad"="Religious Tradition",
                  "richwork"="Would work if rich", 
                  "satfin"="Satisfied with finances", 
                  "satjob"="Satisfied with work",
                  "selfhunt"="Hunter: self",
                  "sexeduc"="Sex-ed. in public schools", 
                  "socbar"="Meet friends at bar",
                  "socfrend"="Meet friends", 
                  "socialscale"="Social life scale",
                  "socommun"="Meet neighbors", 
                  "socrel"="Meet relatives",
                  "spanking"="Spank children", 
                  "spkath"="Athiest: Make speech",
                  "spkcom"="Red: Make speech", 
                  "spkhomo"="Gay: Make speech", 
                  "spkmil"="Militarist: Make speech", 
                  "spkrac"="Racist: Make speech", 
                  "spkmslm"="Islamist: Make speech",
                  "sprtprsn"="Spiritual person",
                  "suicide1"="Suicide: incurable", 
                  "suicide2"="Suicide: bankruptcy",
                  "suicide3"="Suicide: dishonor", 
                  "suicide4"="Suicide: tired of living",
                  "suicidescale"="Suicide scale", 
                  "teensex"="Morality: Sex 14-16 year olds",
                  "thnkself"="Child should think for self", 
                  "trust"="People can be trusted", 
                  "tvhours"="Watch television",
                  "uswary"="US at war next 10 years",
                  "wlthblks"="Blacks rich or poor", 
                  "wlthwhts"="Whites rich or poor",
                  "workblks"="Blacks work hard",
                  "workwhts"="Whites work hard", 
                  "wrkwayup"="No racial favors",
                  "xmarsex"="Morality: Extramarital affair", 
                  "xmovie"="Watch X-rated movie",
                  "workhard"="Child should work hard")) 


save(attitude_vars, file = "~/Desktop/panel_change/data/attitude_vars.Rdata")




