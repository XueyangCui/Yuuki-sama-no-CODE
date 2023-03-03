#常用的化合物参数
library(tidyverse)
tissue_data <- tibble(
  F_C_T = c(0.86, 0.873, 0.675),		# Cells     如果是RBC那么细胞的部分应该是1
  F_B_T = c(0	,0, 0	),  # Blood
  F_W_T = c(0.175, 0.583, 0.530),	  #Water
  F_P_T = c(0.006,0.21, 0.11),	 #Protein
  F_L_T = c(0.855, 0.044, 0.036),		#Lipid
  F_APL_T = c(0.0004, 0.00456, 0.00391),	#Acidic phospholipid
  F_NPL_T = c(0.0016, 0.0238, 0.0123)	,#Neutral phospholipid
  F_NL_T = c(0.853, 0.014, 0.022),	  #Neutral lipid
  pH_T = c(7.1, 7.23, 6.6)			#pH of the tissue
)








#下面开始计算
get_the_distribution_coefficient = function(tissue_data){
  
  logPOW =  3.79  # Octanol: water partition coefficient
  logMA = 2.88	 # Membrane affinity=phosphatidylcholine: water PC (-999 if using logPOW instead of logMA)
  pKa = 8.5	    # pKa for ionization of acid or base
  Alpha = 0.001	# Schmitt estimate of Alpha     用作常数
  F_U_P = 0.05	  # Fraction unbound in plasma
  x = 'base'
  
  #组织的常用pH常数
  pH_P = 7.3			#pH of the plasma
  Hct = 0.46			#Hematocrit = Volume RBC/Volume blood
  F_C_T = F_C_T		# Cells     如果是RBC那么细胞的部分应该是1
  F_B_T = F_B_T		  # Blood
  F_W_T = F_W_T	  #Water
  F_P_T = F_P_T	 #Protein
  F_L_T = F_L_T		#Lipid
  F_APL_T = F_APL_T	#Acidic phospholipid
  F_NPL_T =  F_NPL_T	#Neutral phospholipid
  F_NL_T = F_NL_T	  #Neutral lipid
  pH_T = pH_T			#pH of the tissue
    
  #血浆的组成的成分，按物种一般不需要更改
  F_W_P = 0.96	      #Water
  F_P_P = 0.06	      #Protein
  F_L_P = 0.002		  #Lipid
  F_APL_P = 0.00015		#Acidic phospholipid
  F_NPL_P = 0.0010		#Neutral phospholipid
  F_NL_P = 0.002		#Neutral lipid
 
  #RBC的组成，按物种不需要更改
  F_W_RBC = 0.662		  #Water
  F_P_RBC = 0.33	  	#Protein
  F_L_RBC = 0.005		  # Lipid
  F_APL_RBC = 0.0005	#Acidic phospholipid
  F_NPL_RBC = 0.0029	#Neutral phospholipid
  F_NL_RBC = 0.001		#Neutral lipid
  
   #组织的三成分的归一化
  F_W_T_N = F_W_T / (F_W_T + F_P_T + F_L_T)			#Water
  F_P_T_N = F_P_T / (F_W_T + F_P_T + F_L_T)			#Protein
  F_L_T_N = F_L_T / (F_W_T + F_P_T + F_L_T)			#Lipid
  
  
  #血浆的成分的归一化
  F_W_P_N = F_W_P / (F_W_P + F_P_P + F_L_P)			# Water
  F_P_P_N = F_P_P / (F_W_P + F_P_P + F_L_P)			# Protein
  F_L_P_N = F_L_P / (F_W_P + F_P_P + F_L_P)			# Lipid
  
  #RBC的成分的归一化
  F_W_RBC_N = F_W_RBC / (F_W_RBC + F_P_RBC + F_L_RBC)	#Water
  F_P_RBC_N = F_P_RBC / (F_W_RBC + F_P_RBC + F_L_RBC)	#Protein
  F_L_RBC_N = F_L_RBC / (F_W_RBC + F_P_RBC + F_L_RBC)	#Lipid
  
  #脂质归一化的计算因子，为其占总的组织或者血浆血细胞的百分比，于三种主要成分之中的值。
  Norm_T_L_Fac = (F_APL_T + F_NPL_T + F_NL_T) / F_L_T_N			#Tissue
  Norm_P_L_Fac = (F_APL_P + F_NPL_P + F_NL_P) / F_L_P_N			#Plasma
  Norm_RBC_L_Fac = (F_APL_RBC + F_NPL_RBC + F_NL_RBC) / F_L_RBC_N	# RBC
  
  #Normalize Acidic Phospholipid Fractions
  F_APL_T_N = F_APL_T / Norm_T_L_Fac					#Tissue
  F_APL_P_N = F_APL_P / Norm_P_L_Fac					#Plasma
  F_APL_RBC_N = F_APL_RBC / Norm_RBC_L_Fac		#RBC
  
  #Normalize Neutral Phospholipid Fractions
  F_NPL_T_N = F_NPL_T / Norm_T_L_Fac						#Tissue
  F_NPL_P_N = F_NPL_P / Norm_P_L_Fac					  #Plasma
  F_NPL_RBC_N = F_NPL_RBC / Norm_RBC_L_Fac			#RBC
  
  #Normalize Neutral Lipid Fractions
  F_NL_T_N = F_NL_T / Norm_T_L_Fac						#Tissue
  F_NL_P_N = F_NL_P / Norm_P_L_Fac						#Plasma
  F_NL_RBC_N = F_NL_RBC / Norm_RBC_L_Fac			#RBC
  
  #tissue fraction 在间质液中的体现，   组织由间质液加细胞组成，间质液主要位水加蛋白
  F_I = 1 - F_C_T		#Fraction of  interstitium   in tissue  组织中除了细胞便为间质液
  F_P_I = 0.37 * F_P_P_N	#Fraction of interstitium that is protein 经验公式
  F_W_I = 1 - F_P_I		#Fraction of interstitium that is water  
  
  #Blood Fractions (i.e., fraction of blood that is ...)
  F_W_Bld = F_W_P_N * (1 - Hct) + F_W_RBC_N * Hct				#Water
  F_P_Bld = F_P_P_N * (1 - Hct) + F_P_RBC_N * Hct				#Protein
  F_APL_Bld = F_APL_P_N * (1 - Hct) + F_APL_RBC_N * Hct			#Acidic phospholipids
  F_NPL_Bld = F_NPL_P_N * (1 - Hct) + F_NPL_RBC_N * Hct			#Neutral phospholipids
  F_NL_Bld = F_NL_P_N * (1 - Hct) + F_NL_RBC_N * Hct				#Neutral lipids
  
  
  #Cell Fractions (i.e., fraction of cell that is ...)  细胞由三个组织部分组成，所以对其的分布进行了计算，后续可以考虑去除掉血液的部分
  F_W_C = (F_W_T_N - (F_B_T * F_W_Bld) - (F_W_I * F_I)) / F_C_T     #water
  F_P_C = (F_P_T_N - (F_B_T * F_P_Bld) - (F_P_I * F_I)) / F_C_T			#Protein
  F_APL_C = (F_APL_T_N - (F_B_T * F_APL_Bld)) / F_C_T				#Acidic phospholipids
  F_NPL_C = (F_NPL_T_N - (F_B_T * F_NPL_Bld)) / F_C_T				#Neutral phospholipids
  F_NL_C = (F_NL_T_N - (F_B_T * F_NL_Bld)) / F_C_T			  #Neutral lipids
  F_L_C = F_NL_C + F_NPL_C + F_APL_C						            #Lipid
  
  
  
  
  #计算
  
  # Membrane Affinity (MA) versus P_OW
  POW = 10^logPOW		#Octanol: water partition
  
  if(logMA == -999) {
    K_NPL = POW			#Neutral phospholipid: water PC = Octanol: water PC
  } else{
    MA = 10^logMA			#Phosphatidylcholine: water PC
    K_NPL = MA			  #Neutral phospholipid: water PC = Phosphatidylcholine: water PC
  }
  
  #Calculate Parameters for Acidic, Basic or Neutral Compounds
  
  
  switch (x,
          'acid' = {
            Tmp = 1 + 10^(pH_T - pKa)								#1.0 / F_Neutral for tissue
            Tmp_P = 1 + 10^(pH_P - pKa)							#1.0 / F_Neutral for plasma
            DOW = POW * (((1 - Alpha) / Tmp) + Alpha)		#Distribution coefficient for tissue
            DOW_P = POW * (((1 - Alpha) / Tmp_P) + Alpha)		#Distribution coefficient for plasma
            F_Neutral = 1 / Tmp						#Neutral fraction for tissue
            F_Charged = 1 - F_Neutral			#Charged fraction for tissue
            K_APL = K_NPL * (F_Neutral + (0.05 * F_Charged))	#acidic phospholipid:water PC
            Kappa1 = (((1 - Alpha) / Tmp) + Alpha) / (((1 - Alpha) / Tmp_P) + Alpha)		#Cell: plasma PC for ionizable species
            
          }, 
          'base' = {Tmp = 1 + 10^(pKa - pH_T)								#1.0 / F_Neutral for tissue
          Tmp_P = 1 + 10^(pKa - pH_P)						#1.0 / F_Neutral for plasma
          DOW = POW * (((1 - Alpha) / Tmp) + Alpha)						#Distribution coefficient for tissue
          DOW_P = POW * (((1 - Alpha) / Tmp_P) + Alpha)				#Distribution coefficient for plasma
          F_Neutral = 1 / Tmp								#Neutral fraction for tissue
          F_Charged = 1 - F_Neutral					#Charged fraction for tissue
          K_APL = K_NPL * (F_Neutral + (20 * F_Charged))					#Acidic phospholipid: water PC
          Kappa1 = (((1 - Alpha) / Tmp) + Alpha) / (((1 - Alpha) / Tmp_P) + Alpha)	#Cell: plasma PC for ionizable species 
          },
          'neutral' = {
            DOW = POW										#Distribution coefficient
            K_APL = K_NPL								#Acidic phospholipid: water PC
            Kappa1 = 1.0								#Cell: plasma PC for neutral species 
          }
  )
  #Partition Coefficients
  K_P = 0.163 + (0.0221 * K_NPL)	#Protein: water PC
  K_NL = DOW										  #Neutral lipid: water PC
  K_P_P = (F_W_P_N / F_P_P_N) * ((1.0 / F_U_P) - 1.0)	 #Protein: water PC for plasma
  
  #Fraction Unbound
  F_U_I = 1.0 / (1.0 + F_P_I * K_P_P / F_W_I)									  	 #Interstitium
  F_U_C = F_W_C /(F_W_C + (F_P_C * K_P) + (F_APL_C * K_APL) + (F_NPL_C * K_NPL) + (F_NL_C * K_NL))	 #Cell
  
  #Tissue: Plasma Partition Coefficient
  Ptp = (F_U_P / F_W_P_N) * ((F_I * F_W_I) / F_U_I + (Kappa1 * F_C_T * F_W_C) / F_U_C)
  Ptp
}


pmap_dbl(tissue_data, ~get_the_distribution_coefficient(...))