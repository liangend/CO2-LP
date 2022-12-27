library(lpSolve)
child_food = read.csv("datasets/food_consumption.csv")
food_table = read.csv("datasets/food_table.csv")
food_table[,5:14] = food_table[,5:14] / 100  # convert nutrient/100 g to nutrient/g
food_supp = read.csv("datasets/food_supplement.csv")
food_supp[,5:14] = food_supp[,5:14] / 100
child_anthro = read.csv("datasets/003_anthrochildren.csv")

food_table[is.na(food_table)] = 0
food_supp[is.na(food_supp)] = 0

# food table with CO2 effect
food_table_CO2 = read.csv("datasets/food_table_CO2_200.csv")
food_table_CO2[,5:14] = food_table_CO2[,5:13] / 100
food_supp_CO2 = read.csv("datasets/food_supplement_CO2_200.csv")
food_supp_CO2[,5:14] = food_supp_CO2[,5:13] / 100
food_table_CO2[is.na(food_table_CO2)] = 0
food_supp_CO2[is.na(food_supp_CO2)] = 0

## Calculate the weight of supplement food
wj = c()
for (i in 1:nrow(food_supp)) {
  food_j = child_food[child_food$uniq_ID == food_supp$uniq_ID[i], ]
  NC = length(unique(food_j$idind)) # the number of individuals consuming the jth food
  food_cons_j = aggregate(gcons ~ idind * day, data = food_j, sum)
  Qj = median(food_cons_j$gcons)  # the median quantity (g) of the jth food consumed
  wj[i] = (1 + 1/NC) / Qj
}
food_supp$wj = wj

indid = unique(child_food$idind)
indid = indid[indid %in% child_anthro$idind] # remove individuals without anthropometrics

sol_result = list()
obj_result = c()
status_result = c()

energy_intake_lp = c()
prot_intake_lp = c()
zn_intake_lp = c()
fe_intake_lp = c()
VA_intake_lp = c()
VB12_intake_lp = c()
folate_intake_lp = c()

energy_rice_lp = c()
prot_rice_lp = c()
zn_rice_lp = c()
fe_rice_lp = c()
VA_rice_lp = c()
VB12_rice_lp = c()
folate_rice_lp = c()

energy_wheat_lp = c()
prot_wheat_lp = c()
zn_wheat_lp = c()
fe_wheat_lp = c()
VA_wheat_lp = c()
VB12_wheat_lp = c()
folate_wheat_lp = c()

energy_meat_lp = c()
prot_meat_lp = c()
zn_meat_lp = c()
fe_meat_lp = c()
VA_meat_lp = c()
VB12_meat_lp = c()
folate_meat_lp = c()

energy_egg_lp = c()
prot_egg_lp = c()
zn_egg_lp = c()
fe_egg_lp = c()
VA_egg_lp = c()
VB12_egg_lp = c()
folate_egg_lp = c()

energy_fruit_lp = c()
prot_fruit_lp = c()
zn_fruit_lp = c()
fe_fruit_lp = c()
VA_fruit_lp = c()
VB12_fruit_lp = c()
folate_fruit_lp = c()

energy_veg_lp = c()
prot_veg_lp = c()
zn_veg_lp = c()
fe_veg_lp = c()
VA_veg_lp = c()
VB12_veg_lp = c()
folate_veg_lp = c()

energy_milk_lp = c()
prot_milk_lp = c()
zn_milk_lp = c()
fe_milk_lp = c()
VA_milk_lp = c()
VB12_milk_lp = c()
folate_milk_lp = c()

energy_sugar_lp = c()
prot_sugar_lp = c()
zn_sugar_lp = c()
fe_sugar_lp = c()
VA_sugar_lp = c()
VB12_sugar_lp = c()
folate_sugar_lp = c()

energy_oil_lp = c()
prot_oil_lp = c()
zn_oil_lp = c()
fe_oil_lp = c()
VA_oil_lp = c()
VB12_oil_lp = c()
folate_oil_lp = c()

energy_legume_lp = c()
prot_legume_lp = c()
zn_legume_lp = c()
fe_legume_lp = c()
VA_legume_lp = c()
VB12_legume_lp = c()
folate_legume_lp = c()

zn_intake_CO2_lp = c()
fe_intake_CO2_lp = c()

consume_rice_lp = c()
consume_wheat_lp = c()
consume_meat_lp = c()
consume_egg_lp = c()
consume_fruit_lp = c()
consume_veg_lp = c()
consume_milk_lp = c()
consume_sugar_lp = c()
consume_oil_lp = c()
consume_legume_lp = c()

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
    # food table for individual i
    food_table_i = merge(food_table, food_intake_i, by = "uniq_ID")
    supp_food_i = setdiff(food_supp$uniq_ID, food_intake_i$uniq_ID)
    food_supp_i = food_supp[food_supp$uniq_ID %in% supp_food_i, ]
    
    ### Obj function (cvec)
    cvec = c(rep(c(0,1,1), each = nrow(food_intake_i)), food_supp_i$wj)
    
    ### constraint matrix (Amat)
    ## Nutrient requirement
    # Energy
    energy_c = c(food_table_i$Energy_kcal, rep(0, nrow(food_intake_i) * 2), food_supp_i$Energy_kcal)
    
    # Protein 1, protein requirement
    prot_c1 = c(food_table_i$Prot_g, rep(0, nrow(food_intake_i) * 2), food_supp_i$Prot_g)
    
    # Protein 2, protein energy > 5%, energy content of protein = 4 kcal/g
    prot_c2 = 4 * prot_c1 - 0.05 * energy_c
    
    # Protein 3, protein energy < 20%
    prot_c3 = 4 * prot_c1 - 0.2 * energy_c
    
    # Fat 1, fat energy > 15%, energy content of protein = 9 kcal/g
    fat_c = c(food_table_i$Fat_g, rep(0, nrow(food_intake_i) * 2), food_supp_i$Fat_g)
    fat_c1 = 9 * fat_c - 0.15 * energy_c
    
    # Fat 2, fat energy < 40%
    fat_c2 = 9 * fat_c - 0.4 * energy_c
    
    # Zinc
    zn_c = c(food_table_i$Zn_mg, rep(0, nrow(food_intake_i) * 2), food_supp_i$Zn_mg)
    
    # Iron
    fe_c = c(food_table_i$Fe_mg, rep(0, nrow(food_intake_i) * 2), food_supp_i$Fe_mg)
    
    # Vitamin A
    VA_c = c(food_table_i$VitA_RAE, rep(0, nrow(food_intake_i) * 2), food_supp_i$VitA_RAE)
    
    # Vitamin B12
    VB12_c = c(food_table_i$VitB12_mcg, rep(0, nrow(food_intake_i) * 2), food_supp_i$VitB12_mcg)
    
    # Vitamin B9 (folate)
    folate_c = c(food_table_i$Folate_mcg, rep(0, nrow(food_intake_i) * 2), food_supp_i$Folate_mcg)
    
    # Zinc upper limit 7 mg
    zn_max_c = c(food_table_i$Zn_mg, rep(0, nrow(food_intake_i) * 2), food_supp_i$Zn_mg)
    
    # Fe upper limit 40 mg
    fe_max_c = c(food_table_i$Fe_mg, rep(0, nrow(food_intake_i) * 2), food_supp_i$Fe_mg)
    
    # VA upper limit 600 ug
    VA_max_c = c(food_table_i$VitA_RAE, rep(0, nrow(food_intake_i) * 2), food_supp_i$VitA_RAE)
    
    ## Food limit
    # Rice
    rice_c = c((food_table_i$fgminor == "RICE") * food_table_i$Energy_kcal, rep(0, nrow(food_intake_i) * 2), 
               (food_supp_i$fgminor == "RICE") * food_supp_i$Energy_kcal)
    # Wheat
    wheat_c = c((food_table_i$fgminor == "WHEAT") * food_table_i$Energy_kcal, rep(0, nrow(food_intake_i) * 2), 
                (food_supp_i$fgminor == "WHEAT") * food_supp_i$Energy_kcal)
    # Meat
    meat_c = c((food_table_i$fgmajor == "MEAT") * food_table_i$Energy_kcal, rep(0, nrow(food_intake_i) * 2), 
               (food_supp_i$fgmajor == "MEAT") * food_supp_i$Energy_kcal)
    # Egg
    egg_c = c((food_table_i$fgminor == "EGG") * food_table_i$Energy_kcal, rep(0, nrow(food_intake_i) * 2), 
              (food_supp_i$fgminor == "EGG") * food_supp_i$Energy_kcal)
    # Fruit
    fruit_c = c((food_table_i$fgmajor == "FRUIT") * food_table_i$Energy_kcal, rep(0, nrow(food_intake_i) * 2), 
                (food_supp_i$fgmajor == "FRUIT") * food_supp_i$Energy_kcal)
    # Vegetable
    veg_c = c((food_table_i$fgmajor == "VEG") * food_table_i$Energy_kcal, rep(0, nrow(food_intake_i) * 2), 
              (food_supp_i$fgmajor == "VEG") * food_supp_i$Energy_kcal)
    # Milk
    milk_c = c((food_table_i$fgminor == "MILK") * food_table_i$Energy_kcal, rep(0, nrow(food_intake_i) * 2), 
               (food_supp_i$fgminor == "MILK") * food_supp_i$Energy_kcal)
    # Sugar
    sugar_c = c((food_table_i$fgminor == "SUGAR") * food_table_i$Energy_kcal, rep(0, nrow(food_intake_i) * 2), 
                (food_supp_i$fgminor == "SUGAR") * food_supp_i$Energy_kcal)
    # Oil
    oil_c = c((food_table_i$fgminor == "OIL") * food_table_i$Energy_kcal, rep(0, nrow(food_intake_i) * 2), 
              (food_supp_i$fgminor == "OIL") * food_supp_i$Energy_kcal)
    # Legume
    legume_c = c((food_table_i$fgmajor == "LEGUME") * food_table_i$Energy_kcal, rep(0, nrow(food_intake_i) * 2), 
                 (food_supp_i$fgmajor == "LEGUME") * food_supp_i$Energy_kcal)
    # Spices
    spices_c = c((food_table_i$fgminor == "SPICES") * food_table_i$Energy_kcal, rep(0, nrow(food_intake_i) * 2), 
                 (food_supp_i$fgminor == "SPICES") * food_supp_i$Energy_kcal)
    
    ## Linear transformation
    Amat_t = matrix(rep(0, length(cvec) * nrow(food_table_i)), nrow = nrow(food_table_i), ncol = length(cvec))
    for (j in 1:nrow(Amat_t)) {
      Amat_t[j, j] = 1 / food_intake_i$gcons[j]
      Amat_t[j, j + nrow(food_table_i)] = -1
      Amat_t[j, j + nrow(food_table_i) * 2] = 1
      }
    
    Amat = rbind(energy_c, prot_c1, prot_c2, prot_c3, fat_c1, fat_c2, zn_c, fe_c, VA_c, VB12_c, folate_c, 
                 zn_max_c, fe_max_c, VA_max_c,
                 rice_c, wheat_c, meat_c, egg_c, fruit_c, veg_c, milk_c, sugar_c, oil_c, legume_c, spices_c, 
                 Amat_t)
    
    ### constraint right-hand-side (bvec)
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
    zn_req = 4.3
    fe_req = 7
    VA_req = 300
    VB12_req = 0.9
    folate_req = 150
    zn_max = 7
    fe_max = 40
    VA_max = 600
    
    ## food limits
    energy_major_i = aggregate(gcons * Energy_kcal ~ fgmajor, data = food_table_i, sum)
    energy_minor_i = aggregate(gcons * Energy_kcal ~ fgminor, data = food_table_i, sum)
    # Rice
    if ("RICE" %in% energy_minor_i$fgminor) {
      rice_consumed = energy_minor_i[energy_minor_i$fgminor == "RICE", 2]
      } else {
      rice_consumed = 0
      }
    rice_b = max(800, rice_consumed)
    
    # Wheat
    if ("WHEAT" %in% energy_minor_i$fgminor) {
      wheat_consumed = energy_minor_i[energy_minor_i$fgminor == "WHEAT", 2]
      } else {
      wheat_consumed = 0
      }
    wheat_b = max(100, wheat_consumed)
    # Meat
    if ("MEAT" %in% energy_major_i$fgmajor) {
      meat_consumed = energy_major_i[energy_major_i$fgmajor == "MEAT", 2]
      } else {
      meat_consumed = 0
      }
    meat_b = max(100, meat_consumed)
    # Egg
    if ("EGG" %in% energy_minor_i$fgminor) {
      egg_consumed = energy_minor_i[energy_minor_i$fgminor == "EGG", 2]
      } else {
      egg_consumed = 0
      }
    egg_b = max(100, egg_consumed)
    # Fruit
    if ("FRUIT" %in% energy_major_i$fgmajor) {
      fruit_consumed = energy_major_i[energy_major_i$fgmajor == "FRUIT", 2]
      } else {
      fruit_consumed = 0
      }
    fruit_b = max(50, fruit_consumed)
    # Vegetable
    if ("VEG" %in% energy_major_i$fgmajor) {
      veg_consumed = energy_major_i[energy_major_i$fgmajor == "VEG", 2]
      } else {
      veg_consumed = 0
      }
    veg_b = max(50 + 100, veg_consumed)
    # Milk
    if ("MILK" %in% energy_minor_i$fgminor) {
      milk_consumed = energy_minor_i[energy_minor_i$fgminor == "MILK", 2]
      } else {
      milk_consumed = 0
      }
    milk_b = max(100, milk_consumed)
    # Sugar
    if ("SUGAR" %in% energy_minor_i$fgminor) {
      sugar_consumed = energy_minor_i[energy_minor_i$fgminor == "SUGAR", 2]
      } else {
      sugar_consumed = 0
      }
    sugar_b = max(20, sugar_consumed)
    # Oil
    if ("OIL" %in% energy_minor_i$fgminor) {
      oil_consumed = energy_minor_i[energy_minor_i$fgminor == "OIL", 2]
      } else {
      oil_consumed = 0
      }
    oil_b = max(100, oil_consumed)
    # Legume
    if ("LEGUME" %in% energy_major_i$fgmajor) {
      legume_consumed = energy_major_i[energy_major_i$fgmajor == "LEGUME", 2]
      } else {
      legume_consumed = 0
      }
    legume_b = max(100, legume_consumed)
    
    # Spices
    if ("SPICES" %in% energy_minor_i$fgminor) {
      spices_consumed = energy_minor_i[energy_minor_i$fgminor == "SPICES", 2]
    } else {
      spices_consumed = 0
    }
    spices_b = max(50, spices_consumed)
    
    bvec = c(energy_req, prot_req, 0, 0, 0, 0, zn_req, fe_req, VA_req, VB12_req, folate_req,
             zn_max, fe_max, VA_max,
             rice_b, wheat_b, meat_b, egg_b, fruit_b, veg_b, milk_b, sugar_b, oil_b, legume_b, spices_b, 
             rep(1, nrow(Amat_t)))
    
    ### constraint direction (constdir)
    constdir = c(">", ">", ">", "<", ">", "<", ">", ">", ">", ">", ">", 
                 "<", "<", "<",
                 "<", "<", "<", "<", "<", "<", "<", "<", "<", "<", "<",
                 rep("=", nrow(Amat_t)))
    
    ### solve lp
    sol_i = lp(direction = "min", objective.in = cvec, const.mat = Amat, const.dir = constdir, 
               const.rhs = bvec, compute.sens = T)
    sol_result[[length(sol_result) + 1]] = sol_i
    status_result = c(status_result, sol_i$status)
    obj_result = c(obj_result, sol_i$objval)
    
    energy_intake_lp[length(energy_intake_lp)+1] = sum(sol_i$solution * energy_c)
    prot_intake_lp[length(prot_intake_lp)+1] = sum(sol_i$solution * prot_c1)
    zn_intake_lp[length(zn_intake_lp)+1] = sum(sol_i$solution * zn_c)
    fe_intake_lp[length(fe_intake_lp)+1] = sum(sol_i$solution * fe_c)
    VA_intake_lp[length(VA_intake_lp)+1] = sum(sol_i$solution * VA_c)
    VB12_intake_lp[length(VB12_intake_lp)+1] = sum(sol_i$solution * VB12_c)
    folate_intake_lp[length(folate_intake_lp)+1] = sum(sol_i$solution * folate_c)
    
    ## food consumption of LP diet
    consume_rice_lp[length(consume_rice_lp)+1] = sum(sol_i$solution * c((food_table_i$fgminor == "RICE"), 
                                                                        rep(0, nrow(food_intake_i) * 2), 
                                                                        (food_supp_i$fgminor == "RICE")))
    consume_wheat_lp[length(consume_wheat_lp)+1] = sum(sol_i$solution * c((food_table_i$fgminor == "WHEAT"), 
                                                                          rep(0, nrow(food_intake_i) * 2), 
                                                                          (food_supp_i$fgminor == "WHEAT")))
    consume_meat_lp[length(consume_meat_lp)+1] = sum(sol_i$solution * c((food_table_i$fgmajor == "MEAT"), 
                                                                        rep(0, nrow(food_intake_i) * 2), 
                                                                        (food_supp_i$fgmajor == "MEAT")))
    consume_egg_lp[length(consume_egg_lp)+1] = sum(sol_i$solution * c((food_table_i$fgminor == "EGG"), 
                                                                      rep(0, nrow(food_intake_i) * 2), 
                                                                      (food_supp_i$fgminor == "EGG")))
    consume_fruit_lp[length(consume_fruit_lp)+1] = sum(sol_i$solution * c((food_table_i$fgmajor == "FRUIT"), 
                                                                         rep(0, nrow(food_intake_i) * 2), 
                                                                         (food_supp_i$fgmajor == "FRUIT")))
    consume_veg_lp[length(consume_veg_lp)+1] = sum(sol_i$solution * c((food_table_i$fgmajor == "VEG"), 
                                                                      rep(0, nrow(food_intake_i) * 2), 
                                                                      (food_supp_i$fgmajor == "VEG")))
    consume_milk_lp[length(consume_milk_lp)+1] = sum(sol_i$solution * c((food_table_i$fgminor == "MILK"), 
                                                                        rep(0, nrow(food_intake_i) * 2), 
                                                                        (food_supp_i$fgminor == "MILK")))
    consume_sugar_lp[length(consume_sugar_lp)+1] = sum(sol_i$solution * c((food_table_i$fgminor == "SUGAR"), 
                                                                          rep(0, nrow(food_intake_i) * 2), 
                                                                          (food_supp_i$fgminor == "SUGAR")))
    consume_oil_lp[length(consume_oil_lp)+1] = sum(sol_i$solution * c((food_table_i$fgminor == "OIL"), 
                                                                      rep(0, nrow(food_intake_i) * 2), 
                                                                      (food_supp_i$fgminor == "OIL")))
    consume_legume_lp[length(consume_legume_lp)+1] = sum(sol_i$solution * c((food_table_i$fgmajor == "LEGUME"), 
                                                                            rep(0, nrow(food_intake_i) * 2), 
                                                                            (food_supp_i$fgmajor == "LEGUME")))
    
    ## nutrient intake contribution of LP diet
    # Rice
    energy_rice_lp[length(energy_rice_lp)+1] = 
      sum(sol_i$solution * c((food_table_i$fgminor == "RICE") * food_table_i$Energy_kcal, 
                             rep(0, nrow(food_intake_i) * 2), 
                             (food_supp_i$fgminor == "RICE") * food_supp_i$Energy_kcal))
    prot_rice_lp[length(prot_rice_lp)+1] = 
      sum(sol_i$solution * c((food_table_i$fgminor == "RICE") * food_table_i$Prot_g, 
                             rep(0, nrow(food_intake_i) * 2), 
                             (food_supp_i$fgminor == "RICE") * food_supp_i$Prot_g))
    zn_rice_lp[length(zn_rice_lp)+1] = 
      sum(sol_i$solution * c((food_table_i$fgminor == "RICE") * food_table_i$Zn_mg, 
                             rep(0, nrow(food_intake_i) * 2), 
                             (food_supp_i$fgminor == "RICE") * food_supp_i$Zn_mg))
    fe_rice_lp[length(fe_rice_lp)+1] = 
      sum(sol_i$solution * c((food_table_i$fgminor == "RICE") * food_table_i$Fe_mg, 
                             rep(0, nrow(food_intake_i) * 2), 
                             (food_supp_i$fgminor == "RICE") * food_supp_i$Fe_mg))
    VA_rice_lp[length(VA_rice_lp)+1] = 
      sum(sol_i$solution * c((food_table_i$fgminor == "RICE") * food_table_i$VitA_RAE, 
                             rep(0, nrow(food_intake_i) * 2), 
                             (food_supp_i$fgminor == "RICE") * food_supp_i$VitA_RAE))
    VB12_rice_lp[length(VB12_rice_lp)+1] =
      sum(sol_i$solution * c((food_table_i$fgminor == "RICE") * food_table_i$VitB12_mcg, 
                             rep(0, nrow(food_intake_i) * 2), 
                             (food_supp_i$fgminor == "RICE") * food_supp_i$VitB12_mcg))
    folate_rice_lp[length(folate_rice_lp)+1] = 
      sum(sol_i$solution * c((food_table_i$fgminor == "RICE") * food_table_i$Folate_mcg, 
                             rep(0, nrow(food_intake_i) * 2), 
                             (food_supp_i$fgminor == "RICE") * food_supp_i$Folate_mcg))
    
    # Wheat
    energy_wheat_lp[length(energy_wheat_lp)+1] = 
      sum(sol_i$solution * c((food_table_i$fgminor == "WHEAT") * food_table_i$Energy_kcal, 
                             rep(0, nrow(food_intake_i) * 2), 
                             (food_supp_i$fgminor == "WHEAT") * food_supp_i$Energy_kcal))
    prot_wheat_lp[length(prot_wheat_lp)+1] = 
      sum(sol_i$solution * c((food_table_i$fgminor == "WHEAT") * food_table_i$Prot_g, 
                             rep(0, nrow(food_intake_i) * 2), 
                             (food_supp_i$fgminor == "WHEAT") * food_supp_i$Prot_g))
    zn_wheat_lp[length(zn_wheat_lp)+1] = 
      sum(sol_i$solution * c((food_table_i$fgminor == "WHEAT") * food_table_i$Zn_mg, 
                             rep(0, nrow(food_intake_i) * 2), 
                             (food_supp_i$fgminor == "WHEAT") * food_supp_i$Zn_mg))
    fe_wheat_lp[length(fe_wheat_lp)+1] = 
      sum(sol_i$solution * c((food_table_i$fgminor == "WHEAT") * food_table_i$Fe_mg, 
                             rep(0, nrow(food_intake_i) * 2), 
                             (food_supp_i$fgminor == "WHEAT") * food_supp_i$Fe_mg))
    VA_wheat_lp[length(VA_wheat_lp)+1] = 
      sum(sol_i$solution * c((food_table_i$fgminor == "WHEAT") * food_table_i$VitA_RAE, 
                             rep(0, nrow(food_intake_i) * 2), 
                             (food_supp_i$fgminor == "WHEAT") * food_supp_i$VitA_RAE))
    VB12_wheat_lp[length(VB12_wheat_lp)+1] = 
      sum(sol_i$solution * c((food_table_i$fgminor == "WHEAT") * food_table_i$VitB12_mcg, 
                             rep(0, nrow(food_intake_i) * 2), 
                             (food_supp_i$fgminor == "WHEAT") * food_supp_i$VitB12_mcg))
    folate_wheat_lp[length(folate_wheat_lp)+1] = 
      sum(sol_i$solution * c((food_table_i$fgminor == "WHEAT") * food_table_i$Folate_mcg, 
                             rep(0, nrow(food_intake_i) * 2), 
                             (food_supp_i$fgminor == "WHEAT") * food_supp_i$Folate_mcg))
    
    # Meat
    energy_meat_lp[length(energy_meat_lp)+1] = 
      sum(sol_i$solution * c((food_table_i$fgmajor == "MEAT") * food_table_i$Energy_kcal, 
                             rep(0, nrow(food_intake_i) * 2), 
                             (food_supp_i$fgmajor == "MEAT") * food_supp_i$Energy_kcal))
    prot_meat_lp[length(prot_meat_lp)+1] = 
      sum(sol_i$solution * c((food_table_i$fgmajor == "MEAT") * food_table_i$Prot_g, 
                             rep(0, nrow(food_intake_i) * 2), 
                             (food_supp_i$fgmajor == "MEAT") * food_supp_i$Prot_g))
    zn_meat_lp[length(zn_meat_lp)+1] = 
      sum(sol_i$solution * c((food_table_i$fgmajor == "MEAT") * food_table_i$Zn_mg, 
                             rep(0, nrow(food_intake_i) * 2), 
                             (food_supp_i$fgmajor == "MEAT") * food_supp_i$Zn_mg))
    fe_meat_lp[length(fe_meat_lp)+1] = 
      sum(sol_i$solution * c((food_table_i$fgmajor == "MEAT") * food_table_i$Fe_mg, 
                             rep(0, nrow(food_intake_i) * 2), 
                             (food_supp_i$fgmajor == "MEAT") * food_supp_i$Fe_mg))
    VA_meat_lp[length(VA_meat_lp)+1] = 
      sum(sol_i$solution * c((food_table_i$fgmajor == "MEAT") * food_table_i$VitA_RAE, 
                             rep(0, nrow(food_intake_i) * 2), 
                             (food_supp_i$fgmajor == "MEAT") * food_supp_i$VitA_RAE))
    VB12_meat_lp[length(VB12_meat_lp)+1] = 
      sum(sol_i$solution * c((food_table_i$fgmajor == "MEAT") * food_table_i$VitB12_mcg, 
                             rep(0, nrow(food_intake_i) * 2), 
                             (food_supp_i$fgmajor == "MEAT") * food_supp_i$VitB12_mcg))
    folate_meat_lp[length(folate_meat_lp)+1] = 
      sum(sol_i$solution * c((food_table_i$fgmajor == "MEAT") * food_table_i$Folate_mcg, 
                             rep(0, nrow(food_intake_i) * 2), 
                             (food_supp_i$fgmajor == "MEAT") * food_supp_i$Folate_mcg))
    
    # Egg
    energy_egg_lp[length(energy_egg_lp)+1] = 
      sum(sol_i$solution * c((food_table_i$fgminor == "EGG") * food_table_i$Energy_kcal, 
                             rep(0, nrow(food_intake_i) * 2), 
                             (food_supp_i$fgminor == "EGG") * food_supp_i$Energy_kcal))
    prot_egg_lp[length(prot_egg_lp)+1] = 
      sum(sol_i$solution * c((food_table_i$fgminor == "EGG") * food_table_i$Prot_g, 
                             rep(0, nrow(food_intake_i) * 2), 
                             (food_supp_i$fgminor == "EGG") * food_supp_i$Prot_g))
    zn_egg_lp[length(zn_egg_lp)+1] = 
      sum(sol_i$solution * c((food_table_i$fgminor == "EGG") * food_table_i$Zn_mg, 
                             rep(0, nrow(food_intake_i) * 2), 
                             (food_supp_i$fgminor == "EGG") * food_supp_i$Zn_mg))
    fe_egg_lp[length(fe_egg_lp)+1] = 
      sum(sol_i$solution * c((food_table_i$fgminor == "EGG") * food_table_i$Fe_mg, 
                             rep(0, nrow(food_intake_i) * 2), 
                             (food_supp_i$fgminor == "EGG") * food_supp_i$Fe_mg))
    VA_egg_lp[length(VA_egg_lp)+1] = 
      sum(sol_i$solution * c((food_table_i$fgminor == "EGG") * food_table_i$VitA_RAE, 
                             rep(0, nrow(food_intake_i) * 2), 
                             (food_supp_i$fgminor == "EGG") * food_supp_i$VitA_RAE))
    VB12_egg_lp[length(VB12_egg_lp)+1] = 
      sum(sol_i$solution * c((food_table_i$fgminor == "EGG") * food_table_i$VitB12_mcg, 
                             rep(0, nrow(food_intake_i) * 2), 
                             (food_supp_i$fgminor == "EGG") * food_supp_i$VitB12_mcg))
    folate_egg_lp[length(folate_egg_lp)+1] = 
      sum(sol_i$solution * c((food_table_i$fgminor == "EGG") * food_table_i$Folate_mcg, 
                             rep(0, nrow(food_intake_i) * 2), 
                             (food_supp_i$fgminor == "EGG") * food_supp_i$Folate_mcg))

    # Fruit
    energy_fruit_lp[length(energy_fruit_lp)+1] = 
      sum(sol_i$solution * c((food_table_i$fgmajor == "FRUIT") * food_table_i$Energy_kcal, 
                             rep(0, nrow(food_intake_i) * 2), 
                             (food_supp_i$fgmajor == "FRUIT") * food_supp_i$Energy_kcal))
    prot_fruit_lp[length(prot_fruit_lp)+1] = 
      sum(sol_i$solution * c((food_table_i$fgmajor == "FRUIT") * food_table_i$Prot_g, 
                             rep(0, nrow(food_intake_i) * 2), 
                             (food_supp_i$fgmajor == "FRUIT") * food_supp_i$Prot_g))
    zn_fruit_lp[length(zn_fruit_lp)+1] = 
      sum(sol_i$solution * c((food_table_i$fgmajor == "FRUIT") * food_table_i$Zn_mg, 
                             rep(0, nrow(food_intake_i) * 2), 
                             (food_supp_i$fgmajor == "FRUIT") * food_supp_i$Zn_mg))
    fe_fruit_lp[length(fe_fruit_lp)+1] = 
      sum(sol_i$solution * c((food_table_i$fgmajor == "FRUIT") * food_table_i$Fe_mg, 
                             rep(0, nrow(food_intake_i) * 2), 
                             (food_supp_i$fgmajor == "FRUIT") * food_supp_i$Fe_mg))
    VA_fruit_lp[length(VA_fruit_lp)+1] = 
      sum(sol_i$solution * c((food_table_i$fgmajor == "FRUIT") * food_table_i$VitA_RAE, 
                             rep(0, nrow(food_intake_i) * 2), 
                             (food_supp_i$fgmajor == "FRUIT") * food_supp_i$VitA_RAE))
    VB12_fruit_lp[length(VB12_fruit_lp)+1] = 
      sum(sol_i$solution * c((food_table_i$fgmajor == "FRUIT") * food_table_i$VitB12_mcg, 
                             rep(0, nrow(food_intake_i) * 2), 
                             (food_supp_i$fgmajor == "FRUIT") * food_supp_i$VitB12_mcg))
    folate_fruit_lp[length(folate_fruit_lp)+1] = 
      sum(sol_i$solution * c((food_table_i$fgmajor == "FRUIT") * food_table_i$Folate_mcg, 
                             rep(0, nrow(food_intake_i) * 2), 
                             (food_supp_i$fgmajor == "FRUIT") * food_supp_i$Folate_mcg))
    
    # Vegetable
    energy_veg_lp[length(energy_veg_lp)+1] = 
      sum(sol_i$solution * c((food_table_i$fgmajor == "VEG") * food_table_i$Energy_kcal, 
                             rep(0, nrow(food_intake_i) * 2), 
                             (food_supp_i$fgmajor == "VEG") * food_supp_i$Energy_kcal))
    prot_veg_lp[length(prot_veg_lp)+1] = 
      sum(sol_i$solution * c((food_table_i$fgmajor == "VEG") * food_table_i$Prot_g, 
                             rep(0, nrow(food_intake_i) * 2), 
                             (food_supp_i$fgmajor == "VEG") * food_supp_i$Prot_g))
    zn_veg_lp[length(zn_veg_lp)+1] = 
      sum(sol_i$solution * c((food_table_i$fgmajor == "VEG") * food_table_i$Zn_mg, 
                             rep(0, nrow(food_intake_i) * 2), 
                             (food_supp_i$fgmajor == "VEG") * food_supp_i$Zn_mg))
    fe_veg_lp[length(fe_veg_lp)+1] = 
      sum(sol_i$solution * c((food_table_i$fgmajor == "VEG") * food_table_i$Fe_mg, 
                             rep(0, nrow(food_intake_i) * 2), 
                             (food_supp_i$fgmajor == "VEG") * food_supp_i$Fe_mg))
    VA_veg_lp[length(VA_veg_lp)+1] = 
      sum(sol_i$solution * c((food_table_i$fgmajor == "VEG") * food_table_i$VitA_RAE, 
                             rep(0, nrow(food_intake_i) * 2), 
                             (food_supp_i$fgmajor == "VEG") * food_supp_i$VitA_RAE))
    VB12_veg_lp[length(VB12_veg_lp)+1] = 
      sum(sol_i$solution * c((food_table_i$fgmajor == "VEG") * food_table_i$VitB12_mcg, 
                             rep(0, nrow(food_intake_i) * 2), 
                             (food_supp_i$fgmajor == "VEG") * food_supp_i$VitB12_mcg))
    folate_veg_lp[length(folate_veg_lp)+1] = 
      sum(sol_i$solution * c((food_table_i$fgmajor == "VEG") * food_table_i$Folate_mcg, 
                             rep(0, nrow(food_intake_i) * 2), 
                             (food_supp_i$fgmajor == "VEG") * food_supp_i$Folate_mcg))
    
    # Milk
    energy_milk_lp[length(energy_milk_lp)+1] = 
      sum(sol_i$solution * c((food_table_i$fgminor == "MILK") * food_table_i$Energy_kcal, 
                             rep(0, nrow(food_intake_i) * 2), 
                             (food_supp_i$fgminor == "MILK") * food_supp_i$Energy_kcal))
    prot_milk_lp[length(prot_milk_lp)+1] = 
      sum(sol_i$solution * c((food_table_i$fgminor == "MILK") * food_table_i$Prot_g, 
                             rep(0, nrow(food_intake_i) * 2), 
                             (food_supp_i$fgminor == "MILK") * food_supp_i$Prot_g))
    zn_milk_lp[length(zn_milk_lp)+1] = 
      sum(sol_i$solution * c((food_table_i$fgminor == "MILK") * food_table_i$Zn_mg, 
                             rep(0, nrow(food_intake_i) * 2), 
                             (food_supp_i$fgminor == "MILK") * food_supp_i$Zn_mg))
    fe_milk_lp[length(fe_milk_lp)+1] = 
      sum(sol_i$solution * c((food_table_i$fgminor == "MILK") * food_table_i$Fe_mg, 
                             rep(0, nrow(food_intake_i) * 2), 
                             (food_supp_i$fgminor == "MILK") * food_supp_i$Fe_mg))
    VA_milk_lp[length(VA_milk_lp)+1] = 
      sum(sol_i$solution * c((food_table_i$fgminor == "MILK") * food_table_i$VitA_RAE, 
                             rep(0, nrow(food_intake_i) * 2), 
                             (food_supp_i$fgminor == "MILK") * food_supp_i$VitA_RAE))
    VB12_milk_lp[length(VB12_milk_lp)+1] = 
      sum(sol_i$solution * c((food_table_i$fgminor == "MILK") * food_table_i$VitB12_mcg, 
                             rep(0, nrow(food_intake_i) * 2), 
                             (food_supp_i$fgminor == "MILK") * food_supp_i$VitB12_mcg))
    folate_milk_lp[length(folate_milk_lp)+1] = 
      sum(sol_i$solution * c((food_table_i$fgminor == "MILK") * food_table_i$Folate_mcg, 
                             rep(0, nrow(food_intake_i) * 2), 
                             (food_supp_i$fgminor == "MILK") * food_supp_i$Folate_mcg))
    
    # Sugar
    energy_sugar_lp[length(energy_sugar_lp)+1] = 
      sum(sol_i$solution * c((food_table_i$fgminor == "SUGAR") * food_table_i$Energy_kcal, 
                             rep(0, nrow(food_intake_i) * 2), 
                             (food_supp_i$fgminor == "SUGAR") * food_supp_i$Energy_kcal))
    prot_sugar_lp[length(prot_sugar_lp)+1] = 
      sum(sol_i$solution * c((food_table_i$fgminor == "SUGAR") * food_table_i$Prot_g, 
                             rep(0, nrow(food_intake_i) * 2), 
                             (food_supp_i$fgminor == "SUGAR") * food_supp_i$Prot_g))
    zn_sugar_lp[length(zn_sugar_lp)+1] = 
      sum(sol_i$solution * c((food_table_i$fgminor == "SUGAR") * food_table_i$Zn_mg, 
                             rep(0, nrow(food_intake_i) * 2), 
                             (food_supp_i$fgminor == "SUGAR") * food_supp_i$Zn_mg))
    fe_sugar_lp[length(fe_sugar_lp)+1] = 
      sum(sol_i$solution * c((food_table_i$fgminor == "SUGAR") * food_table_i$Fe_mg, 
                             rep(0, nrow(food_intake_i) * 2), 
                             (food_supp_i$fgminor == "SUGAR") * food_supp_i$Fe_mg))
    VA_sugar_lp[length(VA_sugar_lp)+1] = 
      sum(sol_i$solution * c((food_table_i$fgminor == "SUGAR") * food_table_i$VitA_RAE, 
                             rep(0, nrow(food_intake_i) * 2), 
                             (food_supp_i$fgminor == "SUGAR") * food_supp_i$VitA_RAE))
    VB12_sugar_lp[length(VB12_sugar_lp)+1] = 
      sum(sol_i$solution * c((food_table_i$fgminor == "SUGAR") * food_table_i$VitB12_mcg, 
                             rep(0, nrow(food_intake_i) * 2), 
                             (food_supp_i$fgminor == "SUGAR") * food_supp_i$VitB12_mcg))
    folate_sugar_lp[length(folate_sugar_lp)+1] = 
      sum(sol_i$solution * c((food_table_i$fgminor == "SUGAR") * food_table_i$Folate_mcg, 
                             rep(0, nrow(food_intake_i) * 2), 
                             (food_supp_i$fgminor == "SUGAR") * food_supp_i$Folate_mcg))
    
    # Oil
    energy_oil_lp[length(energy_oil_lp)+1] = 
      sum(sol_i$solution * c((food_table_i$fgminor == "OIL") * food_table_i$Energy_kcal, 
                             rep(0, nrow(food_intake_i) * 2), 
                             (food_supp_i$fgminor == "OIL") * food_supp_i$Energy_kcal))
    prot_oil_lp[length(prot_oil_lp)+1] = 
      sum(sol_i$solution * c((food_table_i$fgminor == "OIL") * food_table_i$Prot_g, 
                             rep(0, nrow(food_intake_i) * 2), 
                             (food_supp_i$fgminor == "OIL") * food_supp_i$Prot_g))
    zn_oil_lp[length(zn_oil_lp)+1] = 
      sum(sol_i$solution * c((food_table_i$fgminor == "OIL") * food_table_i$Zn_mg, 
                             rep(0, nrow(food_intake_i) * 2), 
                             (food_supp_i$fgminor == "OIL") * food_supp_i$Zn_mg))
    fe_oil_lp[length(fe_oil_lp)+1] = 
      sum(sol_i$solution * c((food_table_i$fgminor == "OIL") * food_table_i$Fe_mg, 
                             rep(0, nrow(food_intake_i) * 2), 
                             (food_supp_i$fgminor == "OIL") * food_supp_i$Fe_mg))
    VA_oil_lp[length(VA_oil_lp)+1] = 
      sum(sol_i$solution * c((food_table_i$fgminor == "OIL") * food_table_i$VitA_RAE, 
                             rep(0, nrow(food_intake_i) * 2), 
                             (food_supp_i$fgminor == "OIL") * food_supp_i$VitA_RAE))
    VB12_oil_lp[length(VB12_oil_lp)+1] = 
      sum(sol_i$solution * c((food_table_i$fgminor == "OIL") * food_table_i$VitB12_mcg, 
                             rep(0, nrow(food_intake_i) * 2), 
                             (food_supp_i$fgminor == "OIL") * food_supp_i$VitB12_mcg))
    folate_oil_lp[length(folate_oil_lp)+1] = 
      sum(sol_i$solution * c((food_table_i$fgminor == "OIL") * food_table_i$Folate_mcg, 
                             rep(0, nrow(food_intake_i) * 2), 
                             (food_supp_i$fgminor == "OIL") * food_supp_i$Folate_mcg))
    
    # Legume
    energy_legume_lp[length(energy_legume_lp)+1] = 
      sum(sol_i$solution * c((food_table_i$fgmajor == "LEGUME") * food_table_i$Energy_kcal, 
                             rep(0, nrow(food_intake_i) * 2), 
                             (food_supp_i$fgmajor == "LEGUME") * food_supp_i$Energy_kcal))
    prot_legume_lp[length(prot_legume_lp)+1] = 
      sum(sol_i$solution * c((food_table_i$fgmajor == "LEGUME") * food_table_i$Prot_g, 
                             rep(0, nrow(food_intake_i) * 2), 
                             (food_supp_i$fgmajor == "LEGUME") * food_supp_i$Prot_g))
    zn_legume_lp[length(zn_legume_lp)+1] = 
      sum(sol_i$solution * c((food_table_i$fgmajor == "LEGUME") * food_table_i$Zn_mg, 
                             rep(0, nrow(food_intake_i) * 2), 
                             (food_supp_i$fgmajor == "LEGUME") * food_supp_i$Zn_mg))
    fe_legume_lp[length(fe_legume_lp)+1] = 
      sum(sol_i$solution * c((food_table_i$fgmajor == "LEGUME") * food_table_i$Fe_mg, 
                             rep(0, nrow(food_intake_i) * 2), 
                             (food_supp_i$fgmajor == "LEGUME") * food_supp_i$Fe_mg))
    VA_legume_lp[length(VA_legume_lp)+1] = 
      sum(sol_i$solution * c((food_table_i$fgmajor == "LEGUME") * food_table_i$VitA_RAE, 
                             rep(0, nrow(food_intake_i) * 2), 
                             (food_supp_i$fgmajor == "LEGUME") * food_supp_i$VitA_RAE))
    VB12_legume_lp[length(VB12_legume_lp)+1] = 
      sum(sol_i$solution * c((food_table_i$fgmajor == "LEGUME") * food_table_i$VitB12_mcg, 
                             rep(0, nrow(food_intake_i) * 2), 
                             (food_supp_i$fgmajor == "LEGUME") * food_supp_i$VitB12_mcg))
    folate_legume_lp[length(folate_legume_lp)+1] = 
      sum(sol_i$solution * c((food_table_i$fgmajor == "LEGUME") * food_table_i$Folate_mcg, 
                             rep(0, nrow(food_intake_i) * 2), 
                             (food_supp_i$fgmajor == "LEGUME") * food_supp_i$Folate_mcg))
    
    ## Detect Zn and Fe deficiency when keeping the diets the same
    # food table with CO2 effect for individual i
    food_table_CO2_i = merge(food_table_CO2, food_intake_i, by = "uniq_ID")
    food_supp_CO2_i = food_supp_CO2[food_supp_CO2$uniq_ID %in% supp_food_i, ]
    zn_co2 = c(food_table_CO2_i$Zn_mg, rep(0, nrow(food_intake_i) * 2), food_supp_CO2_i$Zn_mg)
    fe_co2 = c(food_table_CO2_i$Fe_mg, rep(0, nrow(food_intake_i) * 2), food_supp_CO2_i$Fe_mg)
    zn_intake_CO2_lp[length(zn_intake_CO2_lp)+1] = sum(sol_i$solution * zn_co2)
    fe_intake_CO2_lp[length(fe_intake_CO2_lp)+1] = sum(sol_i$solution * fe_co2)
    }
  print(i)
  }




