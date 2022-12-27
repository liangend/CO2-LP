child_food = read.csv("datasets/food_consumption.csv")
food_table = read.csv("datasets/food_table.csv")
food_table[,5:13] = food_table[,5:13] / 100  # convert nutrient/100 g to nutrient/g
food_table[is.na(food_table)] = 0
table(food_table$fgminor)
child_anthro = read.csv("/Users/lijinghui/Desktop/Human_nutrition/dataverse_files/003_anthrochildren.csv")

# food table with CO2 effect
food_table_CO2 = read.csv("/Users/lijinghui/Desktop/Human_nutrition/code/Data_for_sim/Children_dat/food_table_CO2_200.csv")
food_table_CO2[,5:13] = food_table_CO2[,5:13] / 100
food_table_CO2[is.na(food_table_CO2)] = 0

indid = unique(child_food$idind)
indid = indid[indid %in% child_anthro$idind] # remove individuals without anthropometrics

zn_req = 4.3
fe_req = 7
VA_req = 300
VB12_req = 0.9
folate_req = 150

zn_def = c()
fe_def = c()
zn_def_CO2 = c()
fe_def_CO2 = c()

energy_intake = c()
prot_intake = c()
energy_def = c()
prot_def = c()
VA_def = c()
VB12_def = c()
folate_def = c()
energy_rice = c()
prot_rice = c()
zn_rice = c()
fe_rice = c()
VA_rice = c()
VB12_rice = c()
folate_rice = c()

energy_wheat = c()
prot_wheat = c()
zn_wheat = c()
fe_wheat = c()
VA_wheat = c()
VB12_wheat = c()
folate_wheat = c()

energy_meat = c()
prot_meat = c()
zn_meat = c()
fe_meat = c()
VA_meat = c()
VB12_meat = c()
folate_meat = c()

energy_egg = c()
prot_egg = c()
zn_egg = c()
fe_egg = c()
VA_egg = c()
VB12_egg = c()
folate_egg = c()

energy_fruit = c()
prot_fruit = c()
zn_fruit = c()
fe_fruit = c()
VA_fruit = c()
VB12_fruit = c()
folate_fruit = c()

energy_veg = c()
prot_veg = c()
zn_veg = c()
fe_veg = c()
VA_veg = c()
VB12_veg = c()
folate_veg = c()

energy_milk = c()
prot_milk = c()
zn_milk = c()
fe_milk = c()
VA_milk = c()
VB12_milk = c()
folate_milk = c()

energy_sugar = c()
prot_sugar = c()
zn_sugar = c()
fe_sugar = c()
VA_sugar = c()
VB12_sugar = c()
folate_sugar = c()

energy_oil = c()
prot_oil = c()
zn_oil= c()
fe_oil = c()
VA_oil = c()
VB12_oil = c()
folate_oil = c()

energy_legume = c()
prot_legume = c()
zn_legume = c()
fe_legume = c()
VA_legume = c()
VB12_legume = c()
folate_legume = c()

consume_rice = c()
consume_wheat = c()
consume_meat = c()
consume_egg = c()
consume_fruit = c()
consume_veg = c()
consume_milk = c()
consume_sugar = c()
consume_oil = c()
consume_legume = c()

energy_req_all = c()
prot_req_all = c()

for (i in 1:length(indid)) {
  for (day in 1:2) {
    child_food_i = child_food[(child_food$idind == indid[i] & child_food$day == day), ]
    weight_i = child_anthro[which(child_anthro$idind == indid[i]), "weight"]
    height_i = child_anthro[which(child_anthro$idind == indid[i]), "height"]
    age_i = child_anthro[which(child_anthro$idind == indid[i]), "agemoanthro"] / 12
    sex_i = child_anthro[which(child_anthro$idind == indid[i]), "sex"]
    
    # intake of each food
    food_intake_i = aggregate(gcons ~ uniq_ID, data = child_food_i, sum)
    food_intake_i = food_intake_i[food_intake_i$gcons>0, ]
    
    food_table_i = merge(food_table, food_intake_i, by = "uniq_ID")
 
    ## nutrient requirement
    PA = 1.16
    if (age_i <= 36) {
      energy_req = (89 * weight_i - 100) + 20
    } else if (sex_i == 1){
      energy_req = 88.5 - (61.9 * age_i) + PA * (26.7 * weight_i + 903 * height_i) + 20
    } else {
      energy_req = 135.3 - (30.8 * age_i) + PA * (10.0 * weight_i + 934 * height_i) + 20
    }
    prot_req = 1.05 * weight_i
    
    energy_req_all[length(energy_req_all)+1] = energy_req
    prot_req_all[length(prot_req_all)+1] = prot_req
    
    fmajor_i = aggregate(gcons ~ fgmajor, data = child_food_i, sum)
    fminor_i = aggregate(gcons ~ fgminor, data = child_food_i, sum)
    
    
    # Zn and Fe
    zn_intake = sum(food_table_i$gcons * food_table_i$Zn_mg)
    fe_intake = sum(food_table_i$gcons * food_table_i$Fe_mg)
    zn_def[length(zn_def)+1] = zn_intake - zn_req
    fe_def[length(fe_def)+1] = fe_intake - fe_req
    
    food_table_CO2_i = merge(food_table_CO2, food_intake_i, by = "uniq_ID")
    zn_def_CO2[length(zn_def_CO2)+1] = sum(food_table_i$gcons * food_table_CO2_i$Zn_mg) - zn_req
    fe_def_CO2[length(fe_def_CO2)+1] = sum(food_table_i$gcons * food_table_CO2_i$Fe_mg) - fe_req
    
    # Energy
    energy_intake[length(energy_intake)+1] = sum(food_table_i$gcons * food_table_i$Energy_kcal)
    energy_def[length(energy_def)+1] = energy_intake[length(energy_intake)] - energy_req
    
    # Protein
    prot_intake[length(prot_intake)+1] = sum(food_table_i$gcons * food_table_i$Prot_g)
    prot_def[length(prot_def)+1] = prot_intake[length(prot_intake)] - prot_req
    
    # VA
    VA_intake = sum(food_table_i$gcons * food_table_i$VitA_RAE)
    VA_def[length(VA_def)+1] = VA_intake - VA_req
    
    # VB12
    VB12_intake = sum(food_table_i$gcons * food_table_i$VitB12_mcg)
    VB12_def[length(VB12_def)+1] = VB12_intake - VB12_req
    
    # Folate
    folate_intake = sum(food_table_i$gcons * food_table_i$Folate_mcg)
    folate_def[length(folate_def)+1] = folate_intake - folate_req
    
    # nutrient intake proportion

    energy_major = aggregate(gcons * Energy_kcal ~ fgmajor , data = food_table_i, sum)
    energy_minor = aggregate(gcons * Energy_kcal ~ fgminor , data = food_table_i, sum)
    
    prot_major = aggregate(gcons * Prot_g ~ fgmajor , data = food_table_i, sum)
    prot_minor = aggregate(gcons * Prot_g ~ fgminor , data = food_table_i, sum)
    
    zn_major = aggregate(gcons * Zn_mg ~ fgmajor , data = food_table_i, sum)
    zn_minor = aggregate(gcons * Zn_mg ~ fgminor , data = food_table_i, sum)
    
    fe_major = aggregate(gcons * Fe_mg ~ fgmajor , data = food_table_i, sum)
    fe_minor = aggregate(gcons * Fe_mg ~ fgminor , data = food_table_i, sum)
    
    VA_major = aggregate(gcons * VitA_RAE ~ fgmajor , data = food_table_i, sum)
    VA_minor = aggregate(gcons * VitA_RAE ~ fgminor , data = food_table_i, sum)
    
    VB12_major = aggregate(gcons * VitB12_mcg ~ fgmajor , data = food_table_i, sum)
    VB12_minor = aggregate(gcons * VitB12_mcg ~ fgminor , data = food_table_i, sum)
    
    folate_major = aggregate(gcons * Folate_mcg ~ fgmajor , data = food_table_i, sum)
    folate_minor = aggregate(gcons * Folate_mcg ~ fgminor , data = food_table_i, sum)
    
    # Rice
    if ("RICE" %in% food_table_i$fgminor) {
      energy_rice[length(energy_rice)+1] = energy_minor[energy_minor$fgminor == "RICE", 2]
      prot_rice[length(prot_rice)+1] = prot_minor[prot_minor$fgminor == "RICE", 2]
      zn_rice[length(zn_rice)+1] = zn_minor[zn_minor$fgminor == "RICE", 2]
      fe_rice[length(fe_rice)+1] = fe_minor[fe_minor$fgminor == "RICE", 2]
      VA_rice[length(VA_rice)+1] = VA_minor[VA_minor$fgminor == "RICE", 2]
      VB12_rice[length(VB12_rice)+1] = VB12_minor[VB12_minor$fgminor == "RICE", 2]
      folate_rice[length(folate_rice)+1] = folate_minor[folate_minor$fgminor == "RICE", 2]
      consume_rice[length(consume_rice)+1] = sum(food_table_i$gcons[food_table_i$fgminor == "RICE"])
    } else {
      energy_rice[length(energy_rice)+1] = 0
      prot_rice[length(prot_rice)+1] = 0
      zn_rice[length(zn_rice)+1] = 0
      fe_rice[length(fe_rice)+1] = 0
      VA_rice[length(VA_rice)+1] = 0
      VB12_rice[length(VB12_rice)+1] = 0
      folate_rice[length(folate_rice)+1] = 0
      consume_rice[length(consume_rice)+1] = 0
    }
    
    # Wheat
    if ("WHEAT" %in% food_table_i$fgminor) {
      energy_wheat[length(energy_wheat)+1] = energy_minor[energy_minor$fgminor == "WHEAT", 2]
      prot_wheat[length(prot_wheat)+1] = prot_minor[prot_minor$fgminor == "WHEAT", 2]
      zn_wheat[length(zn_wheat)+1] = zn_minor[zn_minor$fgminor == "WHEAT", 2]
      fe_wheat[length(fe_wheat)+1] = fe_minor[fe_minor$fgminor == "WHEAT", 2]
      VA_wheat[length(VA_wheat)+1] = VA_minor[VA_minor$fgminor == "WHEAT", 2]
      VB12_wheat[length(VB12_wheat)+1] = VB12_minor[VB12_minor$fgminor == "WHEAT", 2]
      folate_wheat[length(folate_wheat)+1] = folate_minor[folate_minor$fgminor == "WHEAT", 2]
      consume_wheat[length(consume_wheat)+1] = sum(food_table_i$gcons[food_table_i$fgminor == "WHEAT"])
    } else {
      energy_wheat[length(energy_wheat)+1] = 0
      prot_wheat[length(prot_wheat)+1] = 0
      zn_wheat[length(zn_wheat)+1] = 0
      fe_wheat[length(fe_wheat)+1] = 0
      VA_wheat[length(VA_wheat)+1] = 0
      VB12_wheat[length(VB12_wheat)+1] = 0
      folate_wheat[length(folate_wheat)+1] = 0
      consume_wheat[length(consume_wheat)+1] = 0
    }
    
    # Meat
    if ("MEAT" %in% food_table_i$fgmajor) {
      energy_meat[length(energy_meat)+1] = energy_major[energy_major$fgmajor == "MEAT", 2]
      prot_meat[length(prot_meat)+1] = prot_major[prot_major$fgmajor == "MEAT", 2]
      zn_meat[length(zn_meat)+1] = zn_major[zn_major$fgmajor == "MEAT", 2]
      fe_meat[length(fe_meat)+1] = fe_major[fe_major$fgmajor == "MEAT", 2]
      VA_meat[length(VA_meat)+1] = VA_major[VA_major$fgmajor == "MEAT", 2]
      VB12_meat[length(VB12_meat)+1] = VB12_major[VB12_major$fgmajor == "MEAT", 2]
      folate_meat[length(folate_meat)+1] = folate_major[folate_major$fgmajor == "MEAT", 2]
      consume_meat[length(consume_meat)+1] = sum(food_table_i$gcons[food_table_i$fgmajor == "MEAT"])
    } else {
      energy_meat[length(energy_meat)+1] = 0
      prot_meat[length(prot_meat)+1] = 0
      zn_meat[length(zn_meat)+1] = 0
      fe_meat[length(fe_meat)+1] = 0
      VA_meat[length(VA_meat)+1] = 0
      VB12_meat[length(VB12_meat)+1] = 0
      folate_meat[length(folate_meat)+1] = 0
      consume_meat[length(consume_meat)+1] = 0
    }
    
    # Egg
    if ("EGG" %in% food_table_i$fgminor) {
      energy_egg[length(energy_egg)+1] = energy_minor[energy_minor$fgminor == "EGG", 2]
      prot_egg[length(prot_egg)+1] = prot_minor[prot_minor$fgminor == "EGG", 2]
      zn_egg[length(zn_egg)+1] = zn_minor[zn_minor$fgminor == "EGG", 2]
      fe_egg[length(fe_egg)+1] = fe_minor[fe_minor$fgminor == "EGG", 2]
      VA_egg[length(VA_egg)+1] = VA_minor[VA_minor$fgminor == "EGG", 2]
      VB12_egg[length(VB12_egg)+1] = VB12_minor[VB12_minor$fgminor == "EGG", 2]
      folate_egg[length(folate_egg)+1] = folate_minor[folate_minor$fgminor == "EGG", 2]
      consume_egg[length(consume_egg)+1] = sum(food_table_i$gcons[food_table_i$fgminor == "EGG"])
    } else {
      energy_egg[length(energy_egg)+1] = 0
      prot_egg[length(prot_egg)+1] = 0
      zn_egg[length(zn_egg)+1] = 0
      fe_egg[length(fe_egg)+1] = 0
      VA_egg[length(VA_egg)+1] = 0
      VB12_egg[length(VB12_egg)+1] = 0
      folate_egg[length(folate_egg)+1] = 0
      consume_egg[length(consume_egg)+1] = 0
    }
    
    # Fruit
    if ("FRUIT" %in% food_table_i$fgmajor) {
      energy_fruit[length(energy_fruit)+1] = energy_major[energy_major$fgmajor == "FRUIT", 2]
      prot_fruit[length(prot_fruit)+1] = prot_major[prot_major$fgmajor == "FRUIT", 2]
      zn_fruit[length(zn_fruit)+1] = zn_major[zn_major$fgmajor == "FRUIT", 2]
      fe_fruit[length(fe_fruit)+1] = fe_major[fe_major$fgmajor == "FRUIT", 2]
      VA_fruit[length(VA_fruit)+1] = VA_major[VA_major$fgmajor == "FRUIT", 2]
      VB12_fruit[length(VB12_fruit)+1] = VB12_major[VB12_major$fgmajor == "FRUIT", 2]
      folate_fruit[length(folate_fruit)+1] = folate_major[folate_major$fgmajor == "FRUIT", 2]
      consume_fruit[length(consume_fruit)+1] = sum(food_table_i$gcons[food_table_i$fgmajor == "FRUIT"])
    } else {
      energy_fruit[length(energy_fruit)+1] = 0
      prot_fruit[length(prot_fruit)+1] = 0
      zn_fruit[length(zn_fruit)+1] = 0
      fe_fruit[length(fe_fruit)+1] = 0
      VA_fruit[length(VA_fruit)+1] = 0
      VB12_fruit[length(VB12_fruit)+1] = 0
      folate_fruit[length(folate_fruit)+1] = 0
      consume_fruit[length(consume_fruit)+1] = 0
    }
    
    # Vegetable
    if ("VEG" %in% food_table_i$fgmajor) {
      energy_veg[length(energy_veg)+1] = energy_major[energy_major$fgmajor == "VEG", 2]
      prot_veg[length(prot_veg)+1] = prot_major[prot_major$fgmajor == "VEG", 2]
      zn_veg[length(zn_veg)+1] = zn_major[zn_major$fgmajor == "VEG", 2]
      fe_veg[length(fe_veg)+1] = fe_major[fe_major$fgmajor == "VEG", 2]
      VA_veg[length(VA_veg)+1] = VA_major[VA_major$fgmajor == "VEG", 2]
      VB12_veg[length(VB12_veg)+1] = VB12_major[VB12_major$fgmajor == "VEG", 2]
      folate_veg[length(folate_veg)+1] = folate_major[folate_major$fgmajor == "VEG", 2]
      consume_veg[length(consume_veg)+1] = sum(food_table_i$gcons[food_table_i$fgmajor == "VEG"])
    } else {
      energy_veg[length(energy_veg)+1] = 0
      prot_veg[length(prot_veg)+1] = 0
      zn_veg[length(zn_veg)+1] = 0
      fe_veg[length(fe_veg)+1] = 0
      VA_veg[length(VA_veg)+1] = 0
      VB12_veg[length(VB12_veg)+1] = 0
      folate_veg[length(folate_veg)+1] = 0
      consume_veg[length(consume_veg)+1] = 0
    }
    
    # Milk
    if ("MILK" %in% food_table_i$fgminor) {
      energy_milk[length(energy_milk)+1] = energy_minor[energy_minor$fgminor == "MILK", 2]
      prot_milk[length(prot_milk)+1] = prot_minor[prot_minor$fgminor == "MILK", 2]
      zn_milk[length(zn_milk)+1] = zn_minor[zn_minor$fgminor == "MILK", 2]
      fe_milk[length(fe_milk)+1] = fe_minor[fe_minor$fgminor == "MILK", 2]
      VA_milk[length(VA_milk)+1] = VA_minor[VA_minor$fgminor == "MILK", 2]
      VB12_milk[length(VB12_milk)+1] = VB12_minor[VB12_minor$fgminor == "MILK", 2]
      folate_milk[length(folate_milk)+1] = folate_minor[folate_minor$fgminor == "MILK", 2]
      consume_milk[length(consume_milk)+1] = sum(food_table_i$gcons[food_table_i$fgminor == "MILK"])
    } else {
      energy_milk[length(folate_milk)+1] = 0
      prot_milk[length(folate_milk)+1] = 0
      zn_milk[length(folate_milk)+1] = 0
      fe_milk[length(folate_milk)+1] = 0
      VA_milk[length(folate_milk)+1] = 0
      VB12_milk[length(folate_milk)+1] = 0
      folate_milk[length(folate_milk)+1] = 0
      consume_milk[length(consume_milk)+1] = 0
    }
    
    # Sugar
    if ("SUGAR" %in% food_table_i$fgminor) {
      energy_sugar[length(energy_sugar)+1] = energy_minor[energy_minor$fgminor == "SUGAR", 2]
      prot_sugar[length(prot_sugar)+1] = prot_minor[prot_minor$fgminor == "SUGAR", 2]
      zn_sugar[length(zn_sugar)+1] = zn_minor[zn_minor$fgminor == "SUGAR", 2]
      fe_sugar[length(fe_sugar)+1] = fe_minor[fe_minor$fgminor == "SUGAR", 2]
      VA_sugar[length(VA_sugar)+1] = VA_minor[VA_minor$fgminor == "SUGAR", 2]
      VB12_sugar[length(VB12_sugar)+1] = VB12_minor[VB12_minor$fgminor == "SUGAR", 2]
      folate_sugar[length(folate_sugar)+1] = folate_minor[folate_minor$fgminor == "SUGAR", 2]
      consume_sugar[length(consume_sugar)+1] = sum(food_table_i$gcons[food_table_i$fgminor == "SUGAR"])
    } else {
      energy_sugar[length(energy_sugar)+1] = 0
      prot_sugar[length(prot_sugar)+1] = 0
      zn_sugar[length(zn_sugar)+1] = 0
      fe_sugar[length(fe_sugar)+1] = 0
      VA_sugar[length(VA_sugar)+1] = 0
      VB12_sugar[length(VB12_sugar)+1] = 0
      folate_sugar[length(folate_sugar)+1] = 0
      consume_sugar[length(consume_sugar)+1] = 0
    }
    
    # Oil
    if ("OIL" %in% food_table_i$fgminor) {
      energy_oil[length(energy_oil)+1] = energy_minor[energy_minor$fgminor == "OIL", 2]
      prot_oil[length(prot_oil)+1] = prot_minor[prot_minor$fgminor == "OIL", 2]
      zn_oil[length(zn_oil)+1] = zn_minor[zn_minor$fgminor == "OIL", 2]
      fe_oil[length(fe_oil)+1] = fe_minor[fe_minor$fgminor == "OIL", 2]
      VA_oil[length(VA_oil)+1] = VA_minor[VA_minor$fgminor == "OIL", 2]
      VB12_oil[length(VB12_oil)+1] = VB12_minor[VB12_minor$fgminor == "OIL", 2]
      folate_oil[length(folate_oil)+1] = folate_minor[folate_minor$fgminor == "OIL", 2]
      consume_oil[length(consume_oil)+1] = sum(food_table_i$gcons[food_table_i$fgminor == "OIL"])
    } else {
      energy_oil[length(energy_oil)+1] = 0
      prot_oil[length(prot_oil)+1] = 0
      zn_oil[length(zn_oil)+1] = 0
      fe_oil[length(fe_oil)+1] = 0
      VA_oil[length(VA_oil)+1] = 0
      VB12_oil[length(VB12_oil)+1] = 0
      folate_oil[length(folate_oil)+1] = 0
      consume_oil[length(consume_oil)+1] = 0
    }
    
    # Legume
    if ("LEGUME" %in% food_table_i$fgmajor) {
      energy_legume[length(energy_legume)+1] = energy_major[energy_major$fgmajor == "LEGUME", 2]
      prot_legume[length(prot_legume)+1] = prot_major[prot_major$fgmajor == "LEGUME", 2]
      zn_legume[length(zn_legume)+1] = zn_major[zn_major$fgmajor == "LEGUME", 2]
      fe_legume[length(fe_legume)+1] = fe_major[fe_major$fgmajor == "LEGUME", 2]
      VA_legume[length(VA_legume)+1] = VA_major[VA_major$fgmajor == "LEGUME", 2]
      VB12_legume[length(VB12_legume)+1] = VB12_major[VB12_major$fgmajor == "LEGUME", 2]
      folate_legume[length(folate_legume)+1] = folate_major[folate_major$fgmajor == "LEGUME", 2]
      consume_legume[length(consume_legume)+1] = sum(food_table_i$gcons[food_table_i$fgmajor == "LEGUME"])
    } else {
      energy_legume[length(energy_legume)+1] = 0
      prot_legume[length(prot_legume)+1] = 0
      zn_legume[length(zn_legume)+1] = 0
      fe_legume[length(fe_legume)+1] = 0
      VA_legume[length(VA_legume)+1] = 0
      VB12_legume[length(VB12_legume)+1] = 0
      folate_legume[length(folate_legume)+1] = 0
      consume_legume[length(consume_legume)+1] = 0
      }
    }
  print(i)
  }

#### Plot
## contribution of food to each nutrient
library(ggplot2)

nut_cont_ori = data.frame(food = c("Rice", "Wheat", "Vegetable", "Meat", "Egg", "Oil", "Milk", "Legume", 
                                   "Sugar", "Total"),
                          energy = c(mean(energy_rice), mean(energy_wheat), mean(energy_veg), 
                                     mean(energy_meat), mean(energy_egg), mean(energy_oil), 
                                     mean(energy_milk), 
                                     mean(energy_legume), mean(energy_sugar), mean(energy_intake)),
                          protein = c(mean(prot_rice), mean(prot_wheat), mean(prot_veg), mean(prot_meat),
                                      mean(prot_egg), mean(prot_oil), mean(prot_milk),
                                      mean(prot_legume), mean(prot_sugar), mean(prot_intake)),
                          Zn = c(mean(zn_rice), mean(zn_wheat), mean(zn_veg), mean(zn_meat), mean(zn_egg),
                                 mean(zn_oil), mean(zn_milk), mean(zn_legume), mean(zn_sugar),
                                 mean(zn_def + zn_req)),
                          Fe = c(mean(fe_rice), mean(fe_wheat), mean(fe_veg), mean(fe_meat), mean(fe_egg),
                            mean(fe_oil), mean(fe_milk), mean(fe_legume), mean(fe_sugar),
                            mean(fe_def + fe_req)))
sd(fe_sugar/(fe_def + fe_req), na.rm = T) * 100
nut_cont_ori[5, 2:5] / nut_cont_ori[10, 2:5] * 100
nut_cont_ori[4, 2:5] / nut_cont_ori[10, 2:5] * 100 - nut_cont_ori[5, 2:5] / nut_cont_ori[10, 2:5] * 100
100 - apply(nut_cont_ori[c(1:4, 6:9), 2:5], 2, sum) / nut_cont_ori[10, 2:5] * 100
sd((fe_def + fe_req - fe_rice-fe_wheat-fe_veg-fe_meat-fe_egg-fe_oil-fe_milk -
      fe_legume-fe_sugar)/(fe_def + fe_req),  na.rm = T) * 100
ggplot(nut_cont_ori, aes(x="", y=energy, fill=food)) +
  geom_bar(stat="identity", width=1, color="white") +
  labs(title = "Energy") + coord_polar("y", start=0) + theme_void() +
  theme(legend.position = "none")

ggplot(nut_cont_ori, aes(x="", y=protein, fill=food)) +
  geom_bar(stat="identity", width=1, color="white") +
  labs(title = "Protein") + coord_polar("y", start=0) + theme_void() +
  theme(legend.position = "none")

ggplot(nut_cont_ori, aes(x="", y=Zn, fill=food)) +
  geom_bar(stat="identity", width=1, color="white") +
  labs(title = "Zn") + coord_polar("y", start=0) + theme_void() +
  theme(legend.position = "none")

ggplot(nut_cont_ori, aes(x="", y=Fe, fill=food)) +
  geom_bar(stat="identity", width=1, color="white") +
  labs(title = "Fe") + coord_polar("y", start=0) + theme_void()

apply(nut_cont_ori[,-1], 2, function(x){x/sum(x) * 100})


## Cumulative percentage of nutrient deficiency
nut_intake_ori = data.frame(energy = energy_intake, protein = prot_intake, Zn = zn_def + zn_req,
                            Fe = fe_def + fe_req, Zn_def = zn_def, Fe_def = fe_def,
                            energy_def = energy_def, prot_def = prot_def)
sum(nut_intake_ori$energy_def < 0) / nrow(nut_intake_ori) * 100
sum(nut_intake_ori$prot_def < 0) / nrow(nut_intake_ori) * 100
sum(nut_intake_ori$Zn_def < 0) / nrow(nut_intake_ori) * 100
sum(nut_intake_ori$Fe_def < 0) / nrow(nut_intake_ori) * 100


# energy
ggplot(nut_intake_ori, aes(x=energy_def)) + stat_ecdf(geom = "step", size = 1) +
  labs(title = "Energy", y = "Cumulative %", x = "intake - requirement, kcal") + 
  scale_y_continuous(breaks=seq(0,1,0.25), labels = seq(0,100,25)) + 
  theme(text = element_text(size=15, colour = "black"), 
        axis.text.x = element_text(colour = "black", size = 15),
        axis.text.y = element_text(colour = "black", size = 15),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.position = "none")
sum(energy_def < -energy_req * 0.5) / length(energy_def) * 100
 
# protein
ggplot(nut_intake_ori, aes(x=prot_def)) + stat_ecdf(geom = "step", size = 1) +
  labs(title = "Protein", y = "Cumulative %", x = "intake - requirement, g") + 
  scale_y_continuous(breaks=seq(0,1,0.25), labels = seq(0,100,25)) + 
  theme(text = element_text(size=15, colour = "black"), 
        axis.text.x = element_text(colour = "black", size = 15),
        axis.text.y = element_text(colour = "black", size = 15),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.position = "none")
sum(prot_def < -prot_req * 0.5) / length(prot_def) * 100

# Zn
ggplot(nut_intake_ori, aes(x=Zn_def)) + stat_ecdf(geom = "step", size = 1) +
  labs(title = "Zinc", y = "Cumulative %", x = "intake - requirement, mg") + 
  scale_y_continuous(breaks=seq(0,1,0.25), labels = seq(0,100,25)) + 
  theme(text = element_text(size=15, colour = "black"), 
        axis.text.x = element_text(colour = "black", size = 15),
        axis.text.y = element_text(colour = "black", size = 15),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.position = "none")
sum(nut_intake_ori$Zn_def < -zn_req * 0.5) / length(nut_intake_ori$Zn_def) * 100

# Fe
ggplot(nut_intake_ori, aes(x=Fe_def)) + stat_ecdf(geom = "step", size = 1) +
  labs(title = "Iron", y = "Cumulative %", x = "intake - requirement, mg") + 
  scale_y_continuous(breaks=seq(0,1,0.25), labels = seq(0,100,25)) + 
  theme(text = element_text(size=15, colour = "black"), 
        axis.text.x = element_text(colour = "black", size = 15),
        axis.text.y = element_text(colour = "black", size = 15),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.position = "none")
sum(nut_intake_ori$Fe_def < -fe_req * 0.5) / length(nut_intake_ori$Fe_def) * 100

### Comparison of food consumption (need to run data_for_sim/01_diet_lp.R)
food_consume = data.frame(Food = rep(c("Rice", "Wheat", "Vegetable", "Meat", "Egg", "Oil", "Milk", "Legume", 
                                       "Sugar"), 2),
                          Consumption = c(mean(consume_rice), mean(consume_wheat), mean(consume_veg), 
                                          mean(consume_meat), mean(consume_egg),
                                          mean(consume_oil), mean(consume_milk), 
                                          mean(consume_legume), mean(consume_sugar),
                                          mean(consume_rice_lp), mean(consume_wheat_lp), 
                                          mean(consume_veg_lp), mean(consume_meat_lp),
                                          mean(consume_egg_lp), 
                                          mean(consume_oil_lp), mean(consume_milk_lp), 
                                          mean(consume_legume_lp), mean(consume_sugar_lp)),
                          Diet = rep(c("Base", "LP-optimized"), each = 9),
                          se = c(sd(consume_rice), sd(consume_wheat), sd(consume_veg), 
                                 sd(consume_meat), sd(consume_egg), sd(consume_oil), sd(consume_milk), 
                                 sd(consume_legume), sd(consume_sugar),
                                 sd(consume_rice_lp), sd(consume_wheat_lp), sd(consume_veg_lp), 
                                 sd(consume_meat_lp), sd(consume_egg_lp), 
                                 sd(consume_oil_lp), sd(consume_milk_lp), 
                                 sd(consume_legume_lp), sd(consume_sugar_lp)) / sqrt(length(consume_rice)))

ggplot(data=food_consume, aes(x=Food, y=Consumption, fill=Diet)) +
  geom_errorbar(aes(ymin=Consumption-1, ymax=Consumption+se), width=.35,
                position=position_dodge(.9)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  scale_fill_manual(values=c('black','darkgray')) + 
  labs(x = "", y = "Mean consumption, g") + 
  theme(text = element_text(size=15, colour = "black"), 
        axis.text.x = element_text(colour = "black", size = 15),
        axis.text.y = element_text(colour = "black", size = 15),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

### contribution of food to each nutrient for LP diets
nut_cont_LP = data.frame(food = c("Rice", "Wheat", "Vegetable", "Meat", "Egg", "Oil", "Milk", 
                                  "Legume", "Sugar", "Total"),
                          energy = c(mean(energy_rice_lp), mean(energy_wheat_lp), mean(energy_veg_lp), 
                                     mean(energy_meat_lp), mean(energy_egg_lp),
                                     mean(energy_oil_lp), mean(energy_milk_lp), 
                                     mean(energy_legume_lp), mean(energy_sugar_lp), mean(energy_intake_lp)),
                          protein = c(mean(prot_rice_lp), mean(prot_wheat_lp), mean(prot_veg_lp), 
                                      mean(prot_meat_lp), mean(prot_egg_lp),
                                      mean(prot_oil_lp), mean(prot_milk_lp), 
                                      mean(prot_legume_lp), mean(prot_sugar_lp), mean(prot_intake_lp)),
                          Zn = c(mean(zn_rice_lp), mean(zn_wheat_lp), mean(zn_veg_lp), mean(zn_meat_lp),
                                 mean(zn_egg_lp),
                                 mean(zn_oil_lp), mean(zn_milk_lp), mean(zn_legume_lp), mean(zn_sugar_lp),
                                 mean(zn_intake_lp)),
                          Fe = c(mean(fe_rice_lp), mean(fe_wheat_lp), mean(fe_veg_lp), mean(fe_meat_lp),
                                 mean(fe_egg_lp),
                                 mean(fe_oil_lp), mean(fe_milk_lp), mean(fe_legume_lp), mean(fe_sugar_lp),
                                 mean(fe_intake_lp)))
sd(fe_sugar_lp/fe_intake_lp, na.rm = T) * 100
sd((fe_milk_lp-fe_rice_lp-fe_wheat_lp-fe_veg_lp-fe_meat_lp-fe_egg_lp-
      fe_oil_lp-fe_milk_lp-fe_legume_lp-fe_sugar_lp)/fe_intake_lp, 
   na.rm = T) * 100

nut_cont_LP[5, 2:5] / nut_cont_LP[10, 2:5] * 100
100 - apply(nut_cont_LP[c(1:4, 6:9), 2:5], 2, sum) / nut_cont_LP[10, 2:5] * 100

result = t.test(energy_wheat_lp / energy_intake_lp, energy_wheat / energy_intake)
result$stderr
sd(energy_wheat_lp / energy_intake_lp, na.rm = T) / sqrt(416)

## Cumulative percentage of Zn and Fe deficiency
zn_fe = data.frame(zn_def = c(zn_def, zn_def_CO2, zn_intake_lp-zn_req, zn_intake_CO2_lp-zn_req), 
                   fe_def = c(fe_def, fe_def_CO2, fe_intake_lp-fe_req, fe_intake_CO2_lp-fe_req),
                   Diet = rep(c("Base", "Base under elevated CO2", "LP", "LP under elevated CO2"), 
                              each = 462))
sum(zn_def < 0) / length(zn_def)
sum(zn_def_CO2 < 0) / length(zn_def_CO2)
sum(zn_intake_CO2_lp < zn_req) / length(zn_intake_CO2_lp)

sum(fe_def < 0) / length(fe_def)
sum(fe_def_CO2 < 0) / length(fe_def_CO2)
sum(fe_intake_CO2_lp < fe_req) / length(fe_intake_CO2_lp)
(min(zn_intake_CO2_lp) - zn_req) / zn_req
(min(fe_intake_CO2_lp) - fe_req) / fe_req
sum(sort(zn_intake_CO2_lp[which(zn_intake_CO2_lp < zn_req)] - zn_req) / zn_req * 100 > -5) / 399
sum(sort(fe_intake_CO2_lp[which(fe_intake_CO2_lp < fe_req)] - fe_req) / fe_req * 100 > -2) / 253
zn_req * 0.05
fe_req * 0.02
# Zn
ggplot(zn_fe[1:(462*2), ]) + stat_ecdf(geom = "step", size = 0.7, aes(x=zn_def, lty = Diet)) +
  labs(title = "Zinc", y = "Cumulative %", x = "intake - requirement, mg") + 
  scale_y_continuous(breaks=seq(0,1,0.25), labels = seq(0,100,25)) + 
  theme(text = element_text(size=15, colour = "black"), 
        axis.text.x = element_text(colour = "black", size = 15),
        axis.text.y = element_text(colour = "black", size = 15),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.position = "none")
# Fe
ggplot(zn_fe[1:(462*2), ]) + stat_ecdf(geom = "step", size = 0.7, aes(x=fe_def, lty = Diet)) +
  labs(title = "Iron", y = "Cumulative %", x = "intake - requirement, mg") + 
  scale_y_continuous(breaks=seq(0,1,0.25), labels = seq(0,100,25)) + 
  scale_linetype_manual(values = 1:2,name = "Diet",
                        labels = c("Base", bquote(paste("Base under elevated ",CO[2])))) + 
  theme(text = element_text(size=15, colour = "black"), 
        axis.text.x = element_text(colour = "black", size = 15),
        axis.text.y = element_text(colour = "black", size = 15),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())
  
# Zn in LP
ggplot(zn_fe[(462*2+1):(462*4), ]) + stat_ecdf(geom = "step", size = 0.7, aes(x=zn_def, lty = Diet)) +
  labs(title = "Zinc", y = "Cumulative %", x = "intake - requirement, mg") + 
  scale_y_continuous(breaks=seq(0,1,0.25), labels = seq(0,100,25)) + 
  scale_linetype_manual(values = 3:4) + 
  theme(text = element_text(size=15, colour = "black"), 
        axis.text.x = element_text(colour = "black", size = 15),
        axis.text.y = element_text(colour = "black", size = 15),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.position = "none")
# Fe in LP
ggplot(zn_fe[(462*2+1):(462*4), ]) + stat_ecdf(geom = "step", size = 0.7, aes(x=fe_def, lty = Diet)) +
  labs(title = "Iron", y = "Cumulative %", x = "intake - requirement, mg") + 
  xlim(c(-2.5, 8)) + 
  scale_y_continuous(breaks=seq(0,1,0.25), labels = seq(0,100,25)) + 
  scale_linetype_manual(values = 3:4,name = "Diet",
                        labels = c("LP", bquote(paste("LP under elevated ",CO[2])))) + 
  theme(text = element_text(size=15, colour = "black"), 
        axis.text.x = element_text(colour = "black", size = 15),
        axis.text.y = element_text(colour = "black", size = 15),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())


