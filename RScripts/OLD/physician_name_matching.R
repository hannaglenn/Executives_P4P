library(readr)
library(dplyr)
library(stringr)
library(tidyr)
library(readxl)

# read in names data
executive_names <- read_rds(paste0(created_data_path, "executive_names.rds"))
hospital_pdf_locations <- read_rds(paste0(created_data_path,"hospital_pdf_locations.rds"))

# create an indicator for MD
executive_names <- executive_names %>%
  mutate(doctor = ifelse(title %in% c("md", "dr", "do"), 1, NA)) %>%
  ungroup()
  # 11% of people have a doctor title


# read in npi data of physician names
npidata <- readRDS(paste0(created_data_path, "npidata_names.rds"))

# Filter only to physicians
npidata <- npidata %>%
  filter(t_group=="Allopathic & Osteopathic Physicians")

# get rid of people who don't have names (I don't know why so many of them have NA for name??)
npidata <- npidata %>%
  filter(!(is.na(firstname) & is.na(lastname)))
  # still have a million observations (lost 300k)

# change enumeration date to enumeration year
npidata <- npidata %>%
  mutate(enum_date = as.Date(enum_date,'%m/%d/%Y')) %>%
  mutate(enum_year = as.numeric(format(enum_date,'%Y'))) %>%
  select(-enum_date)

# I don't need group or tax code anymore, just specialty
npidata <- npidata %>%
  select(-t_code, -t_group)

# convert first name and last name to one name line and make sure both are lowercase
npidata <- npidata %>%
  mutate(name = paste0(firstname, " ", lastname)) %>%
  mutate(name = tolower(name))

# look for the max number of one name occurring in npi data
npidata <- npidata %>%
  mutate(count=1) %>%
  group_by(name) %>%
  mutate(sum=sum(count)) %>%
  ungroup()
max <- max(npidata$sum)
  # 136

# create a data set that will be used to store matches 
names_data <- executive_names %>%
  distinct(ein, name, title, doctor) %>%
  mutate(num_matches=NA)

# create list of names in the NPPES data
nppes_list <- paste(as.list(npidata)[["name"]], collapse="|")

# First find out how many matches each name has
for (i in 1:dim(names_data)[1]){
  
  match <- str_extract(nppes_list, paste0("(?<=\\|)(",names_data$name[[i]],")(?=\\|)"))
  
  if (!is.na(match)) {
    match_info <- npidata %>%
      filter(name==match) %>%
      select(npi, name)
    
    num_matches <- dim(match_info)[1]
    
    names_data$num_matches[[i]] <- num_matches
  }
  
  if (is.na(match)) {
    names_data$num_matches[[i]] <- 0
  }
}

# filter to sure matches to get specialty and npi
sure_matches <- names_data %>%
  filter(doctor==1 & num_matches==1) %>%
  select(name, ein)

npidata <- npidata %>%
  select(npi, name, t_class)

sure_matches <- sure_matches %>%
  left_join(npidata, by=c("name"))

# duplicates?
dups <- sure_matches %>%
  mutate(count=1) %>%
  group_by(name, ein) %>%
  mutate(sum=sum(count)) %>%
  ungroup() %>%
  filter(sum>1)

sure_matches <- sure_matches %>%
  distinct()

# join back to executive names
executive_names <- executive_names %>%
  left_join(sure_matches, by=c("name", "ein")) 

# we don't need names of those who say they aren't a doctor and don't match
# set doctor to zero if they are an RN or they don't have a title + num_matches=0
names_data <- names_data %>%
  mutate(doctor = ifelse(title=="rn",0,doctor)) %>%
  mutate(doctor = ifelse(num_matches==0 & is.na(title), 0, doctor))

# filter out if doctor is 0 and no matches
names_data <- names_data %>%
  filter(!(doctor==0 & num_matches==0))

# filter out the sure matches (already joined to data)
names_data <- names_data %>%
  filter(!(doctor==1 & num_matches==1))

# create document of data for RAs to look up ####
# only keep the observations we are unsure about
unsure_names_data <- names_data %>%
  filter(is.na(doctor) | (doctor==1 & is.na(npi))) %>%
  filter(years!="2008")

unsure_names_data_RAs <- names_data %>%
  filter(is.na(doctor)) %>%
  select(ein, name, years)

hosp_names <- hospital_pdf_locations %>%
  distinct(ein, name, sort_name, state) %>%
  rename(hosp_name = name)

unsure_names_data_RAs <- unsure_names_data_RAs %>%
  mutate(ein = as.numeric(ein)) %>%
  left_join(hosp_names, by="ein")

write.csv(unsure_names_data_RAs, paste0(created_data_path, "unsure_names_data.csv"))


# drop eins that have issues with names
cleaned_text <- cleaned_text %>%
  filter(ein!=10223482)

# create data only containing potential doctors
potential_doctors <- cleaned_text %>%
  filter(potential_doctor==1) %>%
  select(year, name, ein, position1, position2)

# I need to bring hospital name back into this to make googling easier
hosp_pdf_locations <- readRDS(paste0(created_data_path, "hospital_pdf_locations.rds"))

hosp_pdf_locations <- hosp_pdf_locations %>%
  distinct(ein, name) %>%
  rename(hosp_name=name)

potential_doctors <- potential_doctors %>%
  left_join(hosp_pdf_locations, by="ein")

potential_doctors <- potential_doctors %>%
  mutate(doctor=NA, specialty=NA, npi=NA)

# manually enter information about names after looking for them on google (including putting a zero if they are not a doctor)
unique_specialties <- unique(npidata$t_class)


potential_doctors <- potential_doctors %>%
  mutate(doctor = ifelse(name=="john carlson" & ein=="10130427",0,doctor),
         doctor = ifelse(name=="susan keiler" & ein=="10211551",0,doctor),
         doctor = ifelse(name=="michael lally" & ein=="10211783",0,doctor),
         doctor = ifelse(name=="arthur blank" & ein=="10211797",0,doctor),
         doctor = ifelse(name=="christina harding" & ein=="10211797",0,doctor),
         doctor = ifelse(name=="stuart cooper" & ein=="10215227",0,doctor),
         doctor = ifelse(name=="john welsh" & ein=="10215227",0,doctor),
         doctor = ifelse(name=="john dalton" & ein=="10217211",0,doctor),
         doctor = ifelse(name=="mary hood" & ein=="10217211",0,doctor),
         doctor = ifelse(name=="glenn martin" & ein=="10217211",0,doctor),
         doctor = ifelse(name=="john cox" & ein=="10219904",0,doctor),
         doctor = ifelse(name=="patricia cook" & ein=="10219904",0,doctor),
         doctor = ifelse(name=="timothy churchill" & ein=="10219904",0,doctor),
         doctor = ifelse(name=="ronald brown" & ein=="10223482",0,doctor),
         doctor = ifelse(name=="ronald daigle" & ein=="10234189",0,doctor),
         doctor = ifelse(name=="randall clark" & ein=="10263628",0,doctor),
         doctor = ifelse(name=="robert schlager" & ein=="10263628",1,doctor),
         npi = ifelse(name=="robert schlager" & ein=="10263628",1467432955,npi),
         specialty = ifelse(name=="robert schlager" & ein=="10263628","Family Medicine",specialty),
         doctor = ifelse(name=="stuart cooper" & ein=="10215227",0,doctor),
         doctor = ifelse(name=="john welsh" & ein=="10215227",0,doctor),
         doctor = ifelse(name=="john dalton" & ein=="10217211",0,doctor),
         doctor = ifelse(name=="mary hood" & ein=="10217211",0,doctor),
         doctor = ifelse(name=="glenn martin" & ein=="10217211",0,doctor),
         doctor = ifelse(name=="john cox" & ein=="10219904",0,doctor),
         doctor = ifelse(name=="patricia cook" & ein=="10219904",0,doctor)
  )


# Bring in finished RA documents ####
Aliyah_data <- read_excel(paste0(created_data_path,"Aliyah.xlsx")) %>%
  select(ein, name, `doctor?`, specialty) %>%
  rename(doctor_checked=`doctor?`)
  
Vidya_data <- read_excel(paste0(created_data_path,"Vidya.xlsx")) %>%
  select(ein, name, `doctor?`, specialty) %>%
  rename(doctor_checked=`doctor?`)

RA_data <- rbind(Aliyah_data, Vidya_data) %>%
  mutate(ein=as.character(ein))

rm(Aliyah_data, Vidya_data, dups, match_info)

# join to names data
names_data <- names_data %>%
  left_join(RA_data, by=c("ein", "name"))

# remove people who don't say they are a doctor and are confirmed not a doctor
names_data <- names_data %>%
  filter(!(is.na(doctor) & doctor_checked==0))

hosp_names <- hospital_pdf_locations %>%
  distinct(ein, name, sort_name, state) %>%
  rename(hosp_name = name)

names_data <- names_data %>%
  mutate(ein = as.numeric(ein)) %>%
  left_join(hosp_names, by="ein")

# manually add EINs of docs I can find ####
executive_names <- executive_names %>%
  mutate(npi=ifelse(ein=="10198331" & name=="katheryn resinbrink", 1124067194, npi),
         npi=ifelse(ein=="10211494" & name=="richard goldstein", 1811967698, npi),
         npi=ifelse(ein=="10211503" & name=="david hyde", 1144288341, npi),
         npi=ifelse(ein=="10215911" & name=="james mullen", 1770663312, npi),
         npi=ifelse(ein=="10217211" & name=="michael palumbo", 1740337229, npi),
         npi=ifelse(ein=="10223482" & name=="jason campbell", 1245336577, npi),
         npi=ifelse(ein=="10223482" & name=="mark kowalski", 1366402588, npi),
         npi=ifelse(ein=="10285286" & name=="david bachman", 1952331712, npi),
         npi=ifelse(ein=="10372148" & name=="jay reynolds", 1821014275, npi),
         npi=ifelse(ein=="10372148" & name=="john beaulieu", 1780604777, npi),
         npi=ifelse(ein=="10646166" & name=="jeffrey bird", 1104815273, npi),
         npi=ifelse(ein=="20222118" & name=="randolph knight", 1942320106, npi),
         npi=ifelse(ein=="20222156" & name=="allen stam", 1932187184, npi),
         npi=ifelse(ein=="20222156" & name=="raymond rabideau", 1023096120, npi),
         npi=ifelse(ein=="20222171" & name=="james mcguire", 1386739555, npi),
         npi=ifelse(ein=="20222171" & name=="james murphy", 1770512121, npi),
         npi=ifelse(ein=="20222171" & name=="john kirk", 1477584811, npi),
         npi=ifelse(ein=="20222171" & name=="steven powell", 1477649168, npi),
         npi=ifelse(ein=="20223321" & name=="patricia pratt", 1730276379, npi),
         npi=ifelse(ein=="20483054" & name=="andrew watt", 1518004464, npi),
         npi=ifelse(ein=="30107300" & name=="chnstopher schmidt", 1770529901, npi),
         npi=ifelse(ein=="30179437" & name=="katrina harris", 1841301629, npi),
         npi=ifelse(ein=="30183721" & name=="catherine schneider", 1972599850, npi),
         npi=ifelse(ein=="30185556" & name=="jennifer ladd", 1912979337, npi),
         npi=ifelse(ein=="30185556" & name=="peter stuart", 1568549285, npi),
         npi=ifelse(ein=="30185556" & name=="thomas moseley", 1003813346, npi),
         npi=ifelse(ein=="30219309" & name=="john brumstead", 1851316855, npi),
         npi=ifelse(ein=="30219309" & name=="melinda estes", 1003986944, npi),
         npi=ifelse(ein=="30266986" & name=="lawrence sullivan", 1891726824, npi),
         npi=ifelse(ein=="30266986" & name=="peter burke", 1326120619, npi),
         npi=ifelse(ein=="42103565" & name=="gregory martin", 1326107657, npi),
         npi=ifelse(ein=="42103565" & name=="gregory mertin", 1326107657, npi),
         npi=ifelse(ein=="42768252" & name=="bnan kelly", 1790735066, npi),
         npi=ifelse(ein=="42768252" & name=="brian kelly", 1790735066, npi),
         npi=ifelse(ein=="42769210" & name=="john stevenson", 1043284789, npi),
         npi=ifelse(ein=="43369649" & name=="steven diaz", 1346201522, npi),
         npi=ifelse(ein=="43775926" & name=="jack cooper", 000, npi),
         npi=ifelse(ein=="50258954" & name=="john murphy", 1336189406, npi),
         npi=ifelse(ein=="60646559" & name=="kenneth rhee", 1770696536, npi),
         npi=ifelse(ein=="60646597" & name=="john murphy", 1508861279, npi),
         npi=ifelse(ein=="60646597" & name=="matthew miller", 1790871150, npi),
         npi=ifelse(ein=="60646597" & name=="william delaney", 1942387634, npi),
         npi=ifelse(ein=="60646599" & name=="douglas waite", 1386641025, npi),
         npi=ifelse(ein=="60646599" & name=="william johnson", 1710057351, npi),
         npi=ifelse(ein=="60646715" & name=="kenneth kurtz", 1851474126, npi),
         npi=ifelse(ein=="60646715" & name=="richard smith", 1689770950, npi),
         npi=ifelse(ein=="60646741" & name=="ted weisman", 1588751150, npi),
         npi=ifelse(ein=="60646917" & name=="andrew snyder", 1972542033, npi),
         npi=ifelse(ein=="60646966" & name=="mary barry", 1285695841, npi),
         npi=ifelse(ein=="111562701" & name=="lawrence smith", 1982741088, npi),
         npi=ifelse(ein=="111631759" & name=="linda brady", 1851500870, npi),
         npi=ifelse(ein=="111633487" & name=="lawrence smith", 1982741088, npi),
         npi=ifelse(ein=="112241326" & name=="lawrence smith", 1982741088, npi),
         npi=ifelse(ein=="113241243" & name=="lawrence smith", 1982741088, npi),
         npi=ifelse(ein=="131740118" & name=="michael rosenberg", 000, npi),
         npi=ifelse(ein=="135596796" & name=="william streck", 1518924422, npi),
         npi=ifelse(ein=="141338373" & name=="norm chapin", 1396769808, npi),
         npi=ifelse(ein=="141338413" & name=="brian mcdermott", 1477707552, npi),
         npi=ifelse(ein=="141338413" & name=="matt dunn", 1700978657, npi),
         npi=ifelse(ein=="141338413" & name=="robert pringle", 1295751048, npi),
         npi=ifelse(ein=="141338471" & name=="kent hall", 1851395354, npi),
         npi=ifelse(ein=="141340054" & name=="chnstian castro", 1497829675, npi),
         npi=ifelse(ein=="141340054" & name=="gina del", 1699778977, npi),
         npi=ifelse(ein=="141364513" & name=="harry davis", 1972597516, npi),
         npi=ifelse(ein=="141364513" & name=="rob demuro", 1801893318, npi),
         npi=ifelse(ein=="141731786" & name=="john broderick", 1588666424, npi),
         npi=ifelse(ein=="146019179" & name=="joel buchalter", 1134142714, npi),
         npi=ifelse(ein=="146019179" & name=="michael nesheiwat", 1093768467, npi),
         npi=ifelse(ein=="146019179" & name=="mitchell cohn", 1528068871, npi),
         npi=ifelse(ein=="150532079" & name=="roger scott", 1144210576, npi),
         npi=ifelse(ein=="150532180" & name=="christine wilson", 1407931975, npi),
         npi=ifelse(ein=="150532180" & name=="james 'brien", 1326233511, npi),
         npi=ifelse(ein=="150532180" & name=="james simcoe", 1346321254, npi),
         npi=ifelse(ein=="150532220" & name=="margaret saterlee", 000, npi),
         npi=ifelse(ein=="150533577" & name=="spencer falcon", 1144279449, npi),
         npi=ifelse(ein=="150533578" & name=="amy grace", 1154322469, npi),
         npi=ifelse(ein=="150533578" & name=="dale adamson", 000, npi),
         npi=ifelse(ein=="150584188" & name=="richard gangem", 1275732893, npi),
         npi=ifelse(ein=="160743029" & name=="michael mernll", 1649205782, npi),
         npi=ifelse(ein=="160743029" & name=="michael merrill", 1649205782, npi),
         npi=ifelse(ein=="160743163" & name=="robert lambert", 1871549105, npi),
         npi=ifelse(ein=="160755799" & name=="mark varallo", 1881669281, npi),
         npi=ifelse(ein=="160772474" & name=="james wild", 1437140985, npi),
         npi=ifelse(ein=="160835446" & name=="dennis 'connor", 000, npi),
         npi=ifelse(ein=="160835446" & name=="dennis connor", 000, npi),
         npi=ifelse(ein=="160835446" & name=="robert lambert", 1871549105, npi),
         npi=ifelse(ein=="161137084" & name=="bruce cusenz", 000, npi),
         npi=ifelse(ein=="161471634" & name=="ankur desai", 1710080593, npi),
         npi=ifelse(ein=="161533232" & name=="david hughes", 000, npi),
         npi=ifelse(ein=="161533232" & name=="james foster", 1730151432, npi),
         npi=ifelse(ein=="161576637" & name=="john sperling", 1003852328, npi),
         npi=ifelse(ein=="205077249" & name=="jeffrey folk", 000, npi),
         npi=ifelse(ein=="208756459" & name=="susan hepker", 1326026501, npi),
         npi=ifelse(ein=="210634562" & name=="james dwyer", 1164469383, npi),
         npi=ifelse(ein=="210662542" & name=="david tarantino", 1285666859, npi),
         npi=ifelse(ein=="222325405" & name=="robert mackenzie", 000, npi),
         npi=ifelse(ein=="222458317" & name=="thomas whalen", 1598778003, npi),
         npi=ifelse(ein=="222520073" & name=="karen ferron", 1235205345, npi),
         npi=ifelse(ein=="222520073" & name=="karen ferront", 1235205345, npi),
         npi=ifelse(ein=="222594672" & name=="david green", 1598733354, npi),
         npi=ifelse(ein=="222594672" & name=="douglas ewing", 1811961428, npi),
         npi=ifelse(ein=="222594672" & name=="greg thesing", 1043214711, npi),
         npi=ifelse(ein=="222594672" & name=="imgrund stephen", 1700823077, npi),
         npi=ifelse(ein=="223524939" & name=="james dwyer", 1164469383, npi),
         npi=ifelse(ein=="230596940" & name=="bogucki alfred", 1740219955, npi),
         npi=ifelse(ein=="230596940" & name=="bucci domenic", 1386873446, npi),
         npi=ifelse(ein=="230596940" & name=="domenic bucci", 1386873446, npi),
         npi=ifelse(ein=="230880420" & name=="thomas curry", 1700823077, npi),
         npi=ifelse(ein=="231352222" & name=="peter hartmann", 1669415691, npi),
         npi=ifelse(ein=="231352222" & name=="steven delavens", 1700823077, npi),
         npi=ifelse(ein=="231370484" & name=="mark jacabson", 1407879554, npi),
         npi=ifelse(ein=="231370484" & name=="mark jacobson", 1407879554, npi),
         npi=ifelse(ein=="231370484" & name=="peter cote", 1366553406, npi),
         npi=ifelse(ein=="231370484" & name=="vincent glielmi", 000, npi),
         npi=ifelse(ein=="231440116" & name=="kishor patel", 1972573426, npi),
         npi=ifelse(ein=="231996150" & name=="gerald maloney", 1275680647, npi),
         npi=ifelse(ein=="237042323" & name=="kenneth marshall", 1922146885, npi),
         npi=ifelse(ein=="237134386" & name=="brian griffin", 1154337020, npi),
         npi=ifelse(ein=="237134386" & name=="pedro hernandez", 1649498569, npi),
         npi=ifelse(ein=="240701920" & name=="alan edwards", 1124003157, npi),
         npi=ifelse(ein=="240795463" & name=="anthony oliva", 1427168004, npi),
         npi=ifelse(ein=="240795959" & name=="albert bothejr", 1710952809, npi),
         npi=ifelse(ein=="240795959" & name=="kenneth wood", 1225099138, npi),
         npi=ifelse(ein=="240795959" & name=="kenneth wqod", 1225099138, npi),
         npi=ifelse(ein=="240795959" & name=="rosemary leening", 1346266905, npi),
         npi=ifelse(ein=="240798681" & name=="pravinchandra patel", 1992749477, npi),
         npi=ifelse(ein=="250965404" & name=="bruce bush", 1235103383, npi),
         npi=ifelse(ein=="250965579" & name=="mario 'alessandro", 1609953918, npi),
         npi=ifelse(ein=="250965598" & name=="charles mackenzie", 000, npi),
         npi=ifelse(ein=="250965598" & name=="john sutton", 1164490025, npi),
         npi=ifelse(ein=="250967480" & name=="michael lally", 1982750238, npi),
         npi=ifelse(ein=="250979346" & name=="gregory sheffo", 000, npi),
         npi=ifelse(ein=="250979346" & name=="kevin tyler", 1366634370, npi),
         npi=ifelse(ein=="250987222" & name=="john ferretti", 1477518777, npi),
         npi=ifelse(ein=="251512436" & name=="david booth", 1144250689, npi),
         npi=ifelse(ein=="251512436" & name=="denise johnson", 1346253846, npi),
         npi=ifelse(ein=="251512436" & name=="franklin mclaughlin", 1427024777, npi),
         npi=ifelse(ein=="251512436" & name=="jason brown", 1205850666, npi),
         npi=ifelse(ein=="251512436" & name=="ron unice", 1083685879, npi),
         npi=ifelse(ein=="251512436" & name=="thomas mitchell", 1336185040, npi),
         npi=ifelse(ein=="260806477" & name=="michael mcgrail", 1831166867, npi),
         npi=ifelse(ein=="261938641" & name=="john gies", 1598729824, npi),
         npi=ifelse(ein=="263975185" & name=="andrew carlson", 1922059252, npi),
         npi=ifelse(ein=="310645626" & name=="steven cox", 1245214303, npi),
         npi=ifelse(ein=="311079309" & name=="jeffrey hoffman", 1831177872, npi),
         npi=ifelse(ein=="311079309" & name=="mark williams", 1891894408, npi),
         npi=ifelse(ein=="311156690" & name=="christopher meyer", 1841238094, npi),
         npi=ifelse(ein=="311156690" & name=="wayne munro", 000, npi),
         npi=ifelse(ein=="311724085" & name=="christopher meyer", 1841238094, npi),
         npi=ifelse(ein=="311724085" & name=="wayne munro", 000, npi),
         npi=ifelse(ein=="340714535" & name=="matthew bernhard", 1366524803, npi),
         npi=ifelse(ein=="340714535" & name=="michael martin", 1740276492, npi),
         npi=ifelse(ein=="340714535" & name=="philip myers", 1427291426, npi),
         npi=ifelse(ein=="340714771" & name=="mark kubina", 1295825396, npi),
         npi=ifelse(ein=="344430849" & name=="robert marshall", 000, npi),
         npi=ifelse(ein=="350835006" & name=="james lowe", 1184771461, npi),
         npi=ifelse(ein=="350867958" & name=="james neal", 1528037207, npi),
         npi=ifelse(ein=="350867958" & name=="jeffrey bird", 1104815273, npi),
         npi=ifelse(ein=="350868133" & name=="michael davenport", 1780619635, npi),
         npi=ifelse(ein=="350892672" & name=="thomas huth", 1639156227, npi),
         npi=ifelse(ein=="350983617" & name=="david williams", 1689705287, npi),
         npi=ifelse(ein=="352090919" & name=="kenneth marshall", 1922146885, npi),
         npi=ifelse(ein=="352228583" & name=="frank powell", 1760466874, npi),
         npi=ifelse(ein=="352362438" & name=="keyur patel", 1770543803, npi),
         npi=ifelse(ein=="352363050" & name=="john cooper", 1881665719, npi),
         npi=ifelse(ein=="356001540" & name=="dan berger", 1831166362, npi),
         npi=ifelse(ein=="356001540" & name=="larry allen", 1942310818, npi),
         npi=ifelse(ein=="361799520" & name=="john wu", 1760732036, npi),
         npi=ifelse(ein=="362200248" & name=="sonia mehta", 1265459895, npi),
         npi=ifelse(ein=="363317416" & name=="kenneth bartholomew", 1639144363, npi),
         npi=ifelse(ein=="363616314" & name=="james lehman", 1629047204, npi),
         npi=ifelse(ein=="363616314" & name=="joseph llohmuller", 1831169135, npi),
         npi=ifelse(ein=="363616314" & name=="robert nelson", 000, npi),
         npi=ifelse(ein=="370618939" & name=="james miller", 1588671168, npi),
         npi=ifelse(ein=="370661223" & name=="rick anderson", 1396772976, npi),
         npi=ifelse(ein=="370813229" & name=="gerard mcshane", 1669553012, npi),
         npi=ifelse(ein=="371058692" & name=="charles lucore", 1821172198, npi),
         npi=ifelse(ein=="371305510" & name=="james probst", 000, npi),
         npi=ifelse(ein=="381218516" & name=="michael smith", 1881684983, npi),
         npi=ifelse(ein=="381360562" & name=="james horton", 1881639839, npi),
         npi=ifelse(ein=="381368347" & name=="ginger william", 1447350269, npi),
         npi=ifelse(ein=="381420304" & name=="william lawrence", 1528037801, npi),
         npi=ifelse(ein=="382383119" & name=="jason white", 000, npi),
         npi=ifelse(ein=="382791823" & name=="dennis lemanskti", 1861490799, npi),
         npi=ifelse(ein=="390286215" & name=="edwin fischer", 1295759306, npi),
         npi=ifelse(ein=="390773970" & name=="andrew dorwat", 1174513659, npi),
         npi=ifelse(ein=="390799566" & name=="will schanhofer", 1730179862, npi),
         npi=ifelse(ein=="390806359" & name=="gregory long", 1720008881, npi),
         npi=ifelse(ein=="390806367" & name=="geoffrey priest", 000, npi),
         npi=ifelse(ein=="390813416" & name=="gregory thompson", 1174527816, npi),
         npi=ifelse(ein=="390813416" & name=="jeffrey thompson", 1285638049, npi),
         npi=ifelse(ein=="390813416" & name=="julio bird", 1407850381, npi),
         npi=ifelse(ein=="390813416" & name=="michael dolan", 1982609186, npi),
         npi=ifelse(ein=="390813416" & name=="stephanie carroll", 1942205356, npi),
         npi=ifelse(ein=="390824015" & name=="gregory long", 1720008881, npi),
         npi=ifelse(ein=="390830664" & name=="gregory long", 1720008881, npi),
         npi=ifelse(ein=="390869788" & name=="gregory long", 1720008881, npi),
         npi=ifelse(ein=="390871113" & name=="gregory long", 1720008881, npi),
         npi=ifelse(ein=="390892183" & name=="patrick mccann", 1063486983, npi),
         npi=ifelse(ein=="390926284" & name=="steven bush", 1598879777, npi),
         npi=ifelse(ein=="391028081" & name=="kenneth klein", 1881681294, npi),
         npi=ifelse(ein=="391370626" & name=="kevin carr", 1467417964, npi),
         npi=ifelse(ein=="391509362" & name=="brian sears", 000, npi),
         npi=ifelse(ein=="391509362" & name=="dean gruner", 000, npi),
         npi=ifelse(ein=="391509362" & name=="gregory long", 1720008881, npi),
         npi=ifelse(ein=="396089134" & name=="gregory long", 1720008881, npi),
         npi=ifelse(ein=="410729979" & name=="kevin croston", 1255306148, npi),
         npi=ifelse(ein=="410785161" & name=="tom bracken", 1154390227, npi),
         npi=ifelse(ein=="410855367" & name=="james hoffman", 1861439853, npi),
         npi=ifelse(ein=="410855367" & name=="james hoffmann", 1861439853, npi),
         npi=ifelse(ein=="421019872" & name=="richard hildebrand", 1073574802, npi),
         npi=ifelse(ein=="421231996" & name=="mark dearden", 1730143397, npi),
         npi=ifelse(ein=="430687077" & name=="beverly johnson", 1336306992, npi),
         npi=ifelse(ein=="431656689" & name=="darren bass", 1316089170, npi),
         npi=ifelse(ein=="440546366" & name=="randall height", 1306832266, npi),
         npi=ifelse(ein=="446005758" & name=="edward henegar", 1588733497, npi),
         npi=ifelse(ein=="450310462" & name=="james vanlooy", 1609962968, npi),
         npi=ifelse(ein=="450310462" & name=="mark siegel", 000, npi),
         npi=ifelse(ein=="450310462" & name=="william mckinnon", 1881786218, npi),
         npi=ifelse(ein=="470379834" & name=="john findley", 1558422899, npi),
         npi=ifelse(ein=="470379836" & name=="cary ward", 1487664447, npi),
         npi=ifelse(ein=="470662290" & name=="james smith", 1548273774, npi),
         npi=ifelse(ein=="470796875" & name=="ed raines", 1568414258, npi),
         npi=ifelse(ein=="470796875" & name=="tod voss", 1689754970, npi),
         npi=ifelse(ein=="480630224" & name=="john ryan", 1811913270, npi),
         npi=ifelse(ein=="480799105" & name=="clay fetsch", 1285648626, npi),
         npi=ifelse(ein=="480799105" & name=="david buller", 1346205267, npi),
         npi=ifelse(ein=="481236156" & name=="barbara brown", 1629074661, npi),
         npi=ifelse(ein=="520591628" & name=="thomas lawrence", 1770582553, npi),
         npi=ifelse(ein=="520591667" & name=="eugene egerton", 1194851063, npi),
         npi=ifelse(ein=="521241835" & name=="barry gross", 1235193210, npi),
         npi=ifelse(ein=="521638026" & name=="donald yablonowitz", 1831192426, npi),
         npi=ifelse(ein=="540506331" & name=="john mcgowan", 1215993696, npi),
         npi=ifelse(ein=="550754713" & name=="christopher colenda", 000, npi),
         npi=ifelse(ein=="560619359" & name=="ward adcock", 1447246228, npi),
         npi=ifelse(ein=="562276994" & name=="brett nicks", 1831173244, npi),
         npi=ifelse(ein=="570359174" & name=="brad mock", 000, npi),
         npi=ifelse(ein=="570468486" & name=="gamble troy", 1982662532, npi),
         npi=ifelse(ein=="581649541" & name=="jonathan morris", 1669571600, npi),
         npi=ifelse(ein=="581649541" & name=="robert jansen", 1003811332, npi),
         npi=ifelse(ein=="581649541" & name=="robin wilson", 1851367163, npi),
         npi=ifelse(ein=="581694098" & name=="james bailey", 1225059819, npi),
         npi=ifelse(ein=="581694098" & name=="samuel johnson", 1457394165, npi),
         npi=ifelse(ein=="582176794" & name=="darrell weldon", 000, npi),
         npi=ifelse(ein=="582179986" & name=="james moore", 1538175641, npi),
         npi=ifelse(ein=="590872594" & name=="guillermo pol", 1053349811, npi),
         npi=ifelse(ein=="590872594" & name=="yvonne johnson", 1518077528, npi),
         npi=ifelse(ein=="590910342" & name=="mark hauser", 1508820465, npi),
         npi=ifelse(ein=="590910342" & name=="michael fili", 1033151113, npi),
         npi=ifelse(ein=="591987355" & name=="elisa brown", 1033277728, npi),
         npi=ifelse(ein=="610461767" & name=="venkata reddy", 1922071653, npi),
         npi=ifelse(ein=="620790132" & name=="amanda grubb", 1396868170, npi),
         npi=ifelse(ein=="621519754" & name=="john reed", 1417996000, npi),
         npi=ifelse(ein=="631058174" & name=="vince bergquist", 1174595912, npi),
         npi=ifelse(ein=="640303091" & name=="robert mobley", 1790008803, npi),
         npi=ifelse(ein=="650232993" & name=="jorge mejia", 1508808734, npi),
         npi=ifelse(ein=="650232993" & name=="steven fletcher", 1063412781, npi),
         npi=ifelse(ein=="710236856" & name=="guy gardner", 000, npi),
         npi=ifelse(ein=="710249735" & name=="james lambert", 1588650303, npi),
         npi=ifelse(ein=="710664687" & name=="david ratcliff", 1205886090, npi),
         npi=ifelse(ein=="710664687" & name=="james newton", 1558336602, npi),
         npi=ifelse(ein=="710797499" & name=="james marsh", 1215027784, npi),
         npi=ifelse(ein=="720423659" & name=="kevin jordan", 1922205889, npi),
         npi=ifelse(ein=="720692834" & name=="mark peters", 000, npi),
         npi=ifelse(ein=="721025017" & name=="ed jeffries", 1912903550, npi),
         npi=ifelse(ein=="730700090" & name=="mark frost", 1518967579, npi),
         npi=ifelse(ein=="730790960" & name=="victor pascual", 1598922908, npi),
         npi=ifelse(ein=="731008550" & name=="william stewart", 1124054986, npi),
         npi=ifelse(ein=="736617937" & name=="john migliaccio", 1457303430, npi),
         npi=ifelse(ein=="736617937" & name=="nancy woodruff", 000, npi),
         npi=ifelse(ein=="741393060" & name=="daniel farray", 1639116288, npi),
         npi=ifelse(ein=="743019849" & name=="david butler", 1770582553, npi),
         npi=ifelse(ein=="751912147" & name=="ashvinhumar patel", 1528129871, npi),
         npi=ifelse(ein=="751912147" & name=="ashwinkumar patel", 1528129871, npi),
         npi=ifelse(ein=="751912147" & name=="john scott", 000, npi),
         npi=ifelse(ein=="810231785" & name=="jeremy blanchard", 1922180959, npi),
         npi=ifelse(ein=="810231787" & name=="stephen bechdolt", 1306919329, npi),
         npi=ifelse(ein=="810240726" & name=="john moreland", 1487608980, npi),
         npi=ifelse(ein=="810475376" & name=="greg hanson", 1437269263, npi),
         npi=ifelse(ein=="810475376" & name=="gregory hanson", 1437269263, npi),
         npi=ifelse(ein=="810600548" & name=="lauren roman", 1194927012, npi),
         npi=ifelse(ein=="840255530" & name=="greg mcauliffe", 1427048198, npi),
         npi=ifelse(ein=="840446259" & name=="david miller", 1497764195, npi),
         npi=ifelse(ein=="841337350" & name=="alex cudkowilcz", 1356539084, npi),
         npi=ifelse(ein=="860320447" & name=="allen dewitt", 1013988492, npi),
         npi=ifelse(ein=="860320447" & name=="jeffrey northup", 1760595342, npi),
         npi=ifelse(ein=="880213754" & name=="max jackson", 1255374716, npi),
         npi=ifelse(ein=="880502320" & name=="richard rodriguez", 000, npi),
         npi=ifelse(ein=="910567263" & name=="william feldmann", 1063433563, npi),
         npi=ifelse(ein=="911351110" & name=="gary kaplan", 1871517797, npi),
         npi=ifelse(ein=="920037099" & name=="william bell", 1992711337, npi),
         npi=ifelse(ein=="930386936" & name=="william hamilton", 1396765038, npi),
         npi=ifelse(ein=="930430029" & name=="marc lewis", 1417022112, npi),
         npi=ifelse(ein=="930579722" & name=="william holloway", 1871610758, npi),
         npi=ifelse(ein=="930602940" & name=="jeffrey absalon", 1225003833, npi),
         npi=ifelse(ein=="940760193" & name=="richard gray", 1821009952, npi),
         npi=ifelse(ein=="111631796" & name=="steven silber", 1396785226, npi),
         npi=ifelse(ein=="131624070" & name=="arthur klein", 1477640555, npi),
         npi=ifelse(ein=="131624070" & name=="lawrence smith", 1982741088, npi),
         npi=ifelse(ein=="131624096" & name=="arthur klein", 1477640555, npi),
         npi=ifelse(ein=="131624096" & name=="david reich", 1063425312, npi),
         npi=ifelse(ein=="131624096" & name=="ira nash", 1326076886, npi),
         npi=ifelse(ein=="131624096" & name=="kenneth david", 1942335880, npi),
         npi=ifelse(ein=="131624096" & name=="kenneth davis", 1942335880, npi),
         npi=ifelse(ein=="131740122" & name=="scott cooper", 1558329953, npi),
         npi=ifelse(ein=="133957095" & name=="anthony gagliard", 1265582746, npi),
         npi=ifelse(ein=="133957095" & name=="robert kelly", 1992827117, npi),
         npi=ifelse(ein=="133957095" & name=="steven corwin", 1649437450, npi),
         npi=ifelse(ein=="141338586" & name=="michael doyle", 1568430262, npi),
         npi=ifelse(ein=="141338586" & name=="stephen katz", 1952309486, npi),
         npi=ifelse(ein=="160743032" & name=="jose acevedo", 1851480529, npi),
         npi=ifelse(ein=="160743905" & name=="robert lambert", 1871549105, npi),
         npi=ifelse(ein=="20315693" & name=="joseph pepe", 1396781613, npi),
         npi=ifelse(ein=="221487307" & name=="mitchell rubinstein", 000, npi),
         npi=ifelse(ein=="221487307" & name=="robert brenner", 1639140148, npi),
         npi=ifelse(ein=="221537688" & name=="anthony granato", 000, npi),
         npi=ifelse(ein=="231352174" & name=="scott levy", 1235199035, npi),
         npi=ifelse(ein=="231352204" & name=="greg sorensen", 1235200239, npi),
         npi=ifelse(ein=="231360851" & name=="michael ader", 1275517112, npi),
         npi=ifelse(ein=="237426300" & name=="george brown", 1164430625, npi),
         npi=ifelse(ein=="251260215" & name=="richard collins", 1992240584, npi),
         npi=ifelse(ein=="251801532" & name=="daniel brooks", 1073638466, npi),
         npi=ifelse(ein=="30183483" & name=="matthew zmurko", 1992773154, npi),
         npi=ifelse(ein=="310537095" & name=="danny bailey", 000, npi),
         npi=ifelse(ein=="311435820" & name=="brian gibler", 1518037357, npi),
         npi=ifelse(ein=="311435820" & name=="kevin joseph", 1275604662, npi),
         npi=ifelse(ein=="331065485" & name=="george brown", 000, npi),
         npi=ifelse(ein=="331065485" & name=="won lee", 1902894793, npi),
         npi=ifelse(ein=="340714756" & name=="kevin cooper", 1649299090, npi),
         npi=ifelse(ein=="341887844" & name=="thomas malone", 1508803206, npi),
         npi=ifelse(ein=="344428256" & name=="neera kanwal", 1164401386, npi),
         npi=ifelse(ein=="350593390" & name=="james porter", 1184643694, npi),
         npi=ifelse(ein=="351955872" & name=="eric williams", 1245295849, npi),
         npi=ifelse(ein=="351955872" & name=="jeffrey lsperring", 1851408363, npi),
         npi=ifelse(ein=="351955872" & name=="jonathan curtright", 1770582553, npi),
         npi=ifelse(ein=="362174823" & name=="anthony perry", 1689623803, npi),
         npi=ifelse(ein=="362174823" & name=="david ansell", 1003918004, npi),
         npi=ifelse(ein=="362174823" & name=="david asnell", 1003918004, npi),
         npi=ifelse(ein=="362174823" & name=="julio silva", 1801862925, npi),
         npi=ifelse(ein=="362340313" & name=="franic lamberta", 1669425377, npi),
         npi=ifelse(ein=="371110690" & name=="david graham", 1386759264, npi),
         npi=ifelse(ein=="381357020" & name=="edward coffey", 1942360813, npi),
         npi=ifelse(ein=="381357020" & name=="john popovich", 1306914833, npi),
         npi=ifelse(ein=="381359087" & name=="scott gibson", 1073554325, npi),
         npi=ifelse(ein=="381359087" & name=="scott larson", 1780618496, npi),
         npi=ifelse(ein=="381434090" & name=="linda peterson", 1619091873, npi),
         npi=ifelse(ein=="382027689" & name=="ray king", 1104868371, npi),
         npi=ifelse(ein=="382418383" & name=="scott larson", 1780618496, npi),
         npi=ifelse(ein=="382418383" & name=="william mayer", 1992072748, npi),
         npi=ifelse(ein=="420680452" & name=="stephen stephenson", 1508850496, npi),
         npi=ifelse(ein=="420698265" & name=="tim horrigan", 1467430181, npi),
         npi=ifelse(ein=="420698265" & name=="timothy horrigan", 1467430181, npi),
         npi=ifelse(ein=="42103575" & name=="thomas higgins", 1689788499, npi),
         npi=ifelse(ein=="42103881" & name=="md rosenberg", 1629015805, npi),
         npi=ifelse(ein=="42103881" & name=="stuart rosenberg", 1629015805, npi),
         npi=ifelse(ein=="42105941" & name=="mark keroack", 000, npi),
         npi=ifelse(ein=="421418847" & name=="joseph llohmuller", 1831169135, npi),
         npi=ifelse(ein=="421418847" & name=="robert nelson", 000, npi),
         npi=ifelse(ein=="421418847" & name=="william langley", 1811993348, npi),
         npi=ifelse(ein=="42790311" & name=="mark keroack", 000, npi),
         npi=ifelse(ein=="430654874" & name=="matt shoemaker", 1770228827, npi),
         npi=ifelse(ein=="430980256" & name=="christopher bowe", 1003816976, npi),
         npi=ifelse(ein=="430980256" & name=="david morton", 1295734200, npi),
         npi=ifelse(ein=="430980256" & name=="patrick garrett", 1669466629, npi),
         npi=ifelse(ein=="43314093" & name=="william barron", 000, npi),
         npi=ifelse(ein=="440545289" & name=="mark laney", 1326091034, npi),
         npi=ifelse(ein=="450233470" & name=="robert groves", 1598873036, npi),
         npi=ifelse(ein=="50258896" & name=="michael dacey", 1477529451, npi),
         npi=ifelse(ein=="50258905" & name=="robert corwin", 000, npi),
         npi=ifelse(ein=="50258914" & name=="terrence mcwilllaams", 1356348585, npi),
         npi=ifelse(ein=="50258914" & name=="terrence mcwilllams", 1356348585, npi),
         npi=ifelse(ein=="60646668" & name=="mark sebastian", 000, npi),
         npi=ifelse(ein=="60646668" & name=="stuart markowitz", 1679559686, npi),
         npi=ifelse(ein=="66068853" & name=="john murphy", 1508861279, npi),
         npi=ifelse(ein=="460319070" & name=="robert allen", 1578518726, npi),
         npi=ifelse(ein=="460319070" & name=="robert houser", 1578518726, npi),
         npi=ifelse(ein=="470376604" & name=="william shiffermiller", 1952408486, npi),
         npi=ifelse(ein=="480774005" & name=="thomas smith", 1639102510, npi),
         npi=ifelse(ein=="520486540" & name=="charles albrecht", 1962465419, npi),
         npi=ifelse(ein=="520486540" & name=="daniel silverman", 000, npi),
         npi=ifelse(ein=="520591685" & name=="richard levine", 1477597664, npi),
         npi=ifelse(ein=="520610545" & name=="robert rothstein", 1750317988, npi),
         npi=ifelse(ein=="520795508" & name=="john miller", 1063642205, npi),
         npi=ifelse(ein=="522087445" & name=="william thomas", 1124185939, npi),
         npi=ifelse(ein=="526049658" & name=="john saunders", 1013029446, npi),
         npi=ifelse(ein=="540506332" & name=="edward murphy", 000, npi),
         npi=ifelse(ein=="540715569" & name=="chris thomson", 1598718850, npi),
         npi=ifelse(ein=="541240646" & name=="david ling", 1053380188, npi),
         npi=ifelse(ein=="541240646" & name=="imran ahmad", 1689889511, npi),
         npi=ifelse(ein=="541240646" & name=="michael mcdermott", 1457300808, npi),
         npi=ifelse(ein=="541240646" & name=="thomas ryan", 1518956754, npi),
         npi=ifelse(ein=="550526150" & name=="elisabeth spangler", 1962718734, npi),
         npi=ifelse(ein=="550526150" & name=="thomas mclilwain", 000, npi),
         npi=ifelse(ein=="550643304" & name=="richard king", 1437136702, npi),
         npi=ifelse(ein=="560552787" & name=="barbara carbone", 000, npi),
         npi=ifelse(ein=="560552787" & name=="john mcconnell", 1841259983, npi),
         npi=ifelse(ein=="560552787" & name=="thomas sibert", 1215177001, npi),
         npi=ifelse(ein=="560585243" & name=="david herman", 1528591484, npi),
         npi=ifelse(ein=="561376950" & name=="david cook", 000, npi),
         npi=ifelse(ein=="561376950" & name=="james lederer", 1942364955, npi),
         npi=ifelse(ein=="581790149" & name=="william waters", 1215972278, npi),
         npi=ifelse(ein=="581790149" & name=="williams waters", 1215972278, npi),
         npi=ifelse(ein=="582002413" & name=="alan bier", 000, npi),
         npi=ifelse(ein=="582296052" & name=="james raymond", 000, npi),
         npi=ifelse(ein=="590624371" & name=="jacqueline linas", 1871604231, npi),
         npi=ifelse(ein=="590624371" & name=="tamar ahmed", 1134126535, npi),
         npi=ifelse(ein=="610444707" & name=="kenneth anderson", 1205800034, npi),
         npi=ifelse(ein=="610458376" & name=="william johnson", 1174634661, npi),
         npi=ifelse(ein=="620528340" & name=="mary dillon", 1326195363, npi),
         npi=ifelse(ein=="630841123" & name=="kenneth brewington", 1639174014, npi),
         npi=ifelse(ein=="721028323" & name=="stephanie mills", 000, npi),
         npi=ifelse(ein=="731192764" & name=="james white", 1518907591, npi),
         npi=ifelse(ein=="73731192764" & name=="philip mosca", 1598768715, npi),
         npi=ifelse(ein=="741152597" & name=="felix brian", 1396894523, npi),
         npi=ifelse(ein=="741152597" & name=="harmohinder kochar", 1720086002, npi),
         npi=ifelse(ein=="741152597" & name=="joel abramowitz", 1447220413, npi),
         npi=ifelse(ein=="741152597" & name=="mary cross", 1932204187, npi),
         npi=ifelse(ein=="741152597" & name=="michael shabot", 1942284724, npi),
         npi=ifelse(ein=="741152597" & name=="susan curling", 1740493626, npi),
         npi=ifelse(ein=="752765566" & name=="amy thompson", 1740493626, npi),
         npi=ifelse(ein=="760590551" & name=="john gillean", 1699822635, npi),
         npi=ifelse(ein=="860110232" & name=="steven lewis", 1255354437, npi),
         npi=ifelse(ein=="866007596" & name=="carl myers", 1942250360, npi),
         npi=ifelse(ein=="866007596" & name=="edward paul", 1619058880, npi),
         npi=ifelse(ein=="910565539" & name=="andrew jacobs", 1043321094, npi),
         npi=ifelse(ein=="910565539" & name=="donna smith", 1649326570, npi),
         npi=ifelse(ein=="910565539" & name=="michael glenn", 1255354080, npi),
         npi=ifelse(ein=="910565539" & name=="steve rupp", 1225004807, npi),
         npi=ifelse(ein=="911858433" & name=="stephen smith", 000, npi),
         npi=ifelse(ein=="930386823" & name=="andrew michaels", 1326018953, npi),
         npi=ifelse(ein=="930386823" & name=="george brown", 1164430625, npi),
         npi=ifelse(ein=="930386823" & name=="geroge brown", 1164430625, npi),
         npi=ifelse(ein=="951643327" & name=="richard afable", 000, npi),
         npi=ifelse(ein=="951934652" & name=="paresh patel", 1043399561, npi),
         npi=ifelse(ein=="60646768" & name=="david buono", 1659321305, npi),
         npi=ifelse(ein=="60646768" & name=="steven godfrey", 000, npi),
         npi=ifelse(ein=="131740130" & name=="michael palumbo", 1174521678, npi),
         npi=ifelse(ein=="160743024" & name=="carlos ortiz", 1295820587, npi),
         npi=ifelse(ein=="160743024" & name=="david baum", 1942360938, npi),
         npi=ifelse(ein=="221750190" & name=="benjamin weinstein", 1215101928, npi),
         npi=ifelse(ein=="230794160" & name=="robert shaver", 1346290160, npi),
         npi=ifelse(ein=="231534300" & name=="anthony colleta", 1699735746, npi),
         npi=ifelse(ein=="231534300" & name=="charles wagner", 1053353920, npi),
         npi=ifelse(ein=="250965274" & name=="thomas mcgill", 000, npi),
         npi=ifelse(ein=="262037695" & name=="denise williams", 000, npi),
         npi=ifelse(ein=="310537122" & name=="alan altman", 1801856661, npi),
         npi=ifelse(ein=="310537122" & name=="john robinson", 1134163686, npi),
         npi=ifelse(ein=="310537122" & name=="robert collins", 1083691521, npi),
         npi=ifelse(ein=="310537122" & name=="stephen blatt", 1730144742, npi),
         npi=ifelse(ein=="311538725" & name=="michael buckley", 000, npi),
         npi=ifelse(ein=="341408846" & name=="richard parker", 1366479057, npi),
         npi=ifelse(ein=="341425870" & name=="michael goler", 000, npi),
         npi=ifelse(ein=="341883284" & name=="kha tran", 1962455733, npi),
         npi=ifelse(ein=="341883284" & name=="khah tran", 1962455733, npi),
         npi=ifelse(ein=="344428218" & name=="eric mast", 1437156296, npi),
         npi=ifelse(ein=="351088640" & name=="clif knight", 000, npi),
         npi=ifelse(ein=="351088640" & name=="randall lee", 1124023072, npi),
         npi=ifelse(ein=="351720796" & name=="kenneth marshall", 1922146885, npi),
         npi=ifelse(ein=="380593405" & name=="david duffy", 1801827647, npi),
         npi=ifelse(ein=="380593405" & name=="william cunningham", 1760447668, npi),
         npi=ifelse(ein=="381360584" & name=="theodore glynn", 1154475523, npi),
         npi=ifelse(ein=="381426919" & name=="david walters", 1609885748, npi),
         npi=ifelse(ein=="382800065" & name=="tony yasick", 1437187986, npi),
         npi=ifelse(ein=="382800065" & name=="william vandervliet", 000, npi),
         npi=ifelse(ein=="390806828" & name=="david carlson", 1134179419, npi),
         npi=ifelse(ein=="390806828" & name=="timothy meyer", 1972592178, npi),
         npi=ifelse(ein=="390837206" & name=="sue turney", 1053393926, npi),
         npi=ifelse(ein=="390910727" & name=="james gardner", 1376818047, npi),
         npi=ifelse(ein=="430662495" & name=="pranav parikh", 1437138997, npi),
         npi=ifelse(ein=="440577118" & name=="john duff", 1689124604, npi),
         npi=ifelse(ein=="440655986" & name=="craig mccoy", 1053312017, npi),
         npi=ifelse(ein=="481226830" & name=="herbert doubek", 000, npi),
         npi=ifelse(ein=="481226833" & name=="barbara brown", 1629074661, npi),
         npi=ifelse(ein=="481226856" & name=="barbara brown", 1629074661, npi),
         npi=ifelse(ein=="510103684" & name=="james newman", 1689645319, npi),
         npi=ifelse(ein=="510103684" & name=="robert laskowski", 1477534907, npi),
         npi=ifelse(ein=="510103684" & name=="timothy gardner", 1336288794, npi),
         npi=ifelse(ein=="521169362" & name=="mitchell schwartz", 1437166386, npi),
         npi=ifelse(ein=="521271901" & name=="gary yates", 000, npi),
         npi=ifelse(ein=="540505913" & name=="james redington", 1154491538, npi),
         npi=ifelse(ein=="540696355" & name=="john wall", 1669474656, npi),
         npi=ifelse(ein=="540696355" & name=="stephen krenytzky", 1770582553, npi),
         npi=ifelse(ein=="560547479" & name=="thomas trahey", 1447256854, npi),
         npi=ifelse(ein=="561376368" & name=="andrew mueller", 1144242553, npi),
         npi=ifelse(ein=="562070036" & name=="eugene washington", 000, npi),
         npi=ifelse(ein=="562070036" & name=="thomas owens", 1821172719, npi),
         npi=ifelse(ein=="570343398" & name=="gene dickerson", 1386698652, npi),
         npi=ifelse(ein=="592314655" & name=="allen weiss", 1093927733, npi),
         npi=ifelse(ein=="611293786" & name=="james taylor", 000, npi),
         npi=ifelse(ein=="611293786" & name=="kenneth marshall", 1689952772, npi),
         npi=ifelse(ein=="620479367" & name=="robin womeodu", 000, npi),
         npi=ifelse(ein=="620479367" & name=="stephen miller", 1558696591, npi),
         npi=ifelse(ein=="810232121" & name=="david chen", 000, npi),
         npi=ifelse(ein=="841262971" & name=="willam neff", 1821086513, npi),
         npi=ifelse(ein=="841262971" & name=="william neff", 1821086513, npi),
         npi=ifelse(ein=="910637400" & name=="greg schroedl", 1386732121, npi),
         npi=ifelse(ein=="952477294" & name=="stephen carney", 1457394918, npi)
         )




# change doctor of NPIs that didn't match
executive_names <- executive_names %>%
  mutate(doctor=ifelse(doctor==1 & is.na(npi),0,doctor)) %>%
  mutate(doctor=ifelse(is.na(doctor),0,doctor))

# join specialty to columns that don't already have it
npidata <- npidata %>%
  select(npi, t_class) %>%
  rename(t_class2=t_class)
executive_names <- executive_names %>%
  left_join(npidata, by="npi")
executive_names <- executive_names %>%
  mutate(t_class=ifelse(is.na(t_class), t_class2, t_class)) %>%
  select(-t_class2)

# save executive names data
saveRDS(executive_names, paste0(created_data_path, "executive_names.rds"))





