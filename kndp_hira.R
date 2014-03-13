setwd('/Users/blue0plus/project/KNDP')

library(data.table)
library(survival)

#kndp = kndp[regi_date<as.Date('2011-01-01')]
#write.csv(kndp, file='kndp_hira.csv', fileEncoding='UTF-8', row.names=F)
kndp = read.csv('kndp_hira.csv')
kndp = data.table(kndp)


#
#데이터 수정 및 추가
#
kndp[ , regi_date:=as.Date(regi_date) ]
kndp[ , first_hypo:=as.Date(first_hypo) ]
kndp[ first_hypo=='2011-01-01', first_hypo:=NA]

#추적 기간 추가
#kndp[, follow_up_days:=as.numeric(as.Date(ifelse(is.na(death_date), '2011-01-01', as.character(death_date)))-regi_date)]

#비저혈당 환자로서 기간 & 저혈당 환자로서 기간 추가
kndp[ !is.na(first_hypo), hypo_bef_days:=as.numeric(as.Date(ifelse(regi_date<first_hypo, as.character(first_hypo), as.character(regi_date)))-regi_date) ]
kndp[ !is.na(first_hypo), hypo_aft_days:= as.numeric(as.Date(ifelse(is.na(death_date), '2011-01-01', as.character(death_date)))-as.Date(ifelse(regi_date<first_hypo, as.character(first_hypo), as.character(regi_date))))]
kndp[ is.na(first_hypo), hypo_bef_days:=as.numeric(as.Date(ifelse(is.na(death_date), '2011-01-01', as.character(death_date)))-regi_date)]
kndp[ is.na(first_hypo), hypo_aft_days:=0]

kndp[ , dm_duration:=2011-dm_f_year ]		#당뇨 기간 추가 2010년 기준
#type 수정
kndp[ , Yr_0_bio_ldl_c:=as.numeric(as.character(Yr_0_bio_ldl_c)) ]		#LDL factor -> numeric
kndp[ , Yr_0_AST:=as.numeric(as.character(Yr_0_AST)) ]	#AST factor -> numeric
kndp[ , Yr_0_ALT:=as.numeric(as.character(Yr_0_ALT)) ]
kndp[ , Yr_0_bio_s_cr:=as.numeric(as.character(Yr_0_bio_s_cr)) ]

#심혈관질환
kndp[ mi_hx_yes == 1 | fibril_hx_yes == 1 | coro_boog_hx_yes == 1 | coro_sten_hx_yes == 1 | coro_exch_hx_yes == 1 | Angina_Hx == '있음' | HF_Hx == '있음', cardiov_disease:= 1 ]
kndp[ mi_hx_yes == 0 & fibril_hx_yes == 0 & coro_boog_hx_yes == 0 & coro_sten_hx_yes == 0 & coro_exch_hx_yes == 0 & Angina_Hx == '없음' & HF_Hx == '없음', cardiov_disease:= 0 ]
#뇌혈관 질환
kndp[ cva_hemo_hx_yes == 1 | cva_infa_hx_yes == 1 | cva_tia_hx_yes == 1 | cva_unkn_hx_yes == 1, cerebrov_disease:=1 ]
kndp[ cva_hemo_hx_yes == 0 & cva_infa_hx_yes == 0 & cva_tia_hx_yes == 0 & cva_unkn_hx_yes == 0, cerebrov_disease:=0 ]
#말초혈관질환
kndp[ peri_hx_yes == 1 | PAD_Sx == '있음', periv_disease:=1 ]
kndp[ peri_hx_yes == 0 | PAD_Sx == ' 없음', periv_disease:=0 ]

kndp[ Yr_0_bio_s_cr>100, Yr_0_bio_s_cr:=NA]								#creatinine 극단적으로 큰 수치165 하나 제거

kndp[ is.na(hypo_bef_cost_filter), hypo_bef_cost_filter:=0 ]
kndp[ is.na(hypo_aft_cost_filter), hypo_aft_cost_filter:=0 ]

kndp[ is.na(hypo_bef_vscn_in_filter), hypo_bef_vscn_in_filter:=0 ]
kndp[ is.na(hypo_aft_vscn_in_filter), hypo_aft_vscn_in_filter:=0 ]
kndp[ is.na(hypo_bef_vscn_out_filter), hypo_bef_vscn_out_filter:=0 ]
kndp[ is.na(hypo_aft_vscn_out_filter), hypo_aft_vscn_out_filter:=0 ]

kndp[ is.na(hypo_bef_recn_in_filter), hypo_bef_recn_in_filter:=0 ]
kndp[ is.na(hypo_aft_recn_in_filter), hypo_aft_recn_in_filter:=0 ]
kndp[ is.na(hypo_bef_recn_out_filter), hypo_bef_recn_out_filter:=0 ]
kndp[ is.na(hypo_aft_recn_out_filter), hypo_aft_recn_out_filter:=0 ]


#....column name 전달하는 function이.....잘 안됨 -_-ㅋ
j.stat = function(field){
	print(field)
	t.test(kndp[ !is.na(first_hypo), eval(field) ], kndp[ is.na(first_hypo), eval(field) ])
	kndp[ !is.na(first_hypo), sd(eval(field), na.rm=T) ]
	#kndp[ is.na(first_hypo), list(nonHypo.sd=sd(eval(field), na.rm=T)) ]
	#kndp [ , list(total.mean=mean(eval(field), na.rm=T), total.sd=sd(eval(field), na.rm=T)) ]
}


#
#기본 통계
#
t.test( kndp[ !is.na(first_hypo), (hypo_bef_days + hypo_aft_days)/365 ], kndp[ is.na(first_hypo), (hypo_bef_days + hypo_aft_days)/365 ] )		#추적기간
kndp[ !is.na(first_hypo), sd(hypo_bef_days + hypo_aft_days) ]/365
kndp[ is.na(first_hypo), sd(hypo_bef_days + hypo_aft_days) ]/365
kndp [ , list(mean=mean(hypo_bef_days + hypo_aft_days), sd=sd(hypo_bef_days + hypo_aft_days)) ]/365

t.test( kndp[ !is.na(first_hypo), age2008 ], kndp[ is.na(first_hypo), age2008 ] )		#나이
kndp[ !is.na(first_hypo), sd(age2008) ]
kndp[ is.na(first_hypo), sd(age2008) ]
kndp [ , list(mean=mean(age2008), sd=sd(age2008)) ]

nrow(kndp[first_hypo>=regi_date])/kndp[ , sum(hypo_bef_days)]*365*1000			#저혈당 발병률 1000persons-year
t.test( kndp[!is.na(first_hypo), dm_duration], kndp[is.na(first_hypo), dm_duration] )		#당뇨 기간
kndp[ !is.na(first_hypo), sd(dm_duration, na.rm=T) ]
kndp[ is.na(first_hypo), sd(dm_duration, na.rm=T) ]
kndp [ , list(mean=mean(dm_duration, na.rm=T), sd=sd(dm_duration, na.rm=T)) ]
table(kndp[ !is.na(first_hypo), gender])																											#성별
table(kndp[ is.na(first_hypo), gender ])
fisher.test(matrix(c(75, 1850, 68, 2412), nrow=2, dimnames=list(c('hypo', 'non-hypo'), c('F', 'M'))))
table(kndp$gender)		

t.test(kndp[ !is.na(first_hypo), Yr_0_bio_bmi ], kndp[ is.na(first_hypo), Yr_0_bio_bmi ])					#bmi
kndp[ !is.na(first_hypo), list(hypo.sd=sd(Yr_0_bio_bmi, na.rm=T)) ]
kndp[ is.na(first_hypo), list(nonHypo.sd=sd(Yr_0_bio_bmi, na.rm=T)) ]
kndp [ , list(total.mean=mean(Yr_0_bio_bmi, na.rm=T), total.sd=sd(Yr_0_bio_bmi, na.rm=T)) ]
t.test(kndp[ !is.na(first_hypo), Yr_0_bio_waist_circ ], kndp[ is.na(first_hypo), Yr_0_bio_waist_circ ])					#허리둘레
kndp[ !is.na(first_hypo), list(hypo.sd=sd(Yr_0_bio_waist_circ, na.rm=T)) ]
kndp[ is.na(first_hypo), list(nonHypo.sd=sd(Yr_0_bio_waist_circ, na.rm=T)) ]
kndp [ , list(total.mean=mean(Yr_0_bio_waist_circ, na.rm=T), total.sd=sd(Yr_0_bio_waist_circ, na.rm=T)) ]
t.test(kndp[ !is.na(first_hypo), Yr_0_bio_sys_bp ], kndp[ is.na(first_hypo), Yr_0_bio_sys_bp ])					#수축기 혈압
kndp[ !is.na(first_hypo), list(hypo.sd=sd(Yr_0_bio_sys_bp, na.rm=T)) ]
kndp[ is.na(first_hypo), list(nonHypo.sd=sd(Yr_0_bio_sys_bp, na.rm=T)) ]
kndp [ , list(total.mean=mean(Yr_0_bio_sys_bp, na.rm=T), total.sd=sd(Yr_0_bio_sys_bp, na.rm=T)) ]
t.test(kndp[ !is.na(first_hypo), Yr_0_bio_dia_bp ], kndp[ is.na(first_hypo), Yr_0_bio_dia_bp ])					#이완기 혈압
kndp[ !is.na(first_hypo), list(hypo.sd=sd(Yr_0_bio_dia_bp, na.rm=T)) ]
kndp[ is.na(first_hypo), list(nonHypo.sd=sd(Yr_0_bio_dia_bp, na.rm=T)) ]
kndp [ , list(total.mean=mean(Yr_0_bio_dia_bp, na.rm=T), total.sd=sd(Yr_0_bio_dia_bp, na.rm=T)) ]

t.test(kndp[ !is.na(first_hypo), Yr_0_bio_fbs ], kndp[ is.na(first_hypo), Yr_0_bio_fbs ])					#공복 혈당
kndp[ !is.na(first_hypo), list(hypo.sd=sd(Yr_0_bio_fbs, na.rm=T)) ]
kndp[ is.na(first_hypo), list(nonHypo.sd=sd(Yr_0_bio_fbs, na.rm=T)) ]
kndp [ , list(total.mean=mean(Yr_0_bio_fbs, na.rm=T), total.sd=sd(Yr_0_bio_fbs, na.rm=T)) ]
t.test(kndp[ !is.na(first_hypo), Yr_0_bio_bs_2hr ], kndp[ is.na(first_hypo), Yr_0_bio_bs_2hr ])					#식후 2시간 혈당
kndp[ !is.na(first_hypo), list(hypo.sd=sd(Yr_0_bio_bs_2hr, na.rm=T)) ]
kndp[ is.na(first_hypo), list(nonHypo.sd=sd(Yr_0_bio_bs_2hr, na.rm=T)) ]
kndp [ , list(total.mean=mean(Yr_0_bio_bs_2hr, na.rm=T), total.sd=sd(Yr_0_bio_bs_2hr, na.rm=T)) ]
t.test(kndp[ !is.na(first_hypo), Yr_0_bio_hba1c ], kndp[ is.na(first_hypo), Yr_0_bio_hba1c ])					#당화혈색소
kndp[ !is.na(first_hypo), list(hypo.sd=sd(Yr_0_bio_hba1c, na.rm=T)) ]
kndp[ is.na(first_hypo), list(nonHypo.sd=sd(Yr_0_bio_hba1c, na.rm=T)) ]
kndp [ , list(total.mean=mean(Yr_0_bio_hba1c, na.rm=T), total.sd=sd(Yr_0_bio_hba1c, na.rm=T)) ]
t.test(kndp[ !is.na(first_hypo), Yr_0_bio_t_chol ], kndp[ is.na(first_hypo), Yr_0_bio_t_chol ])					#총 콜레스테롤
kndp[ !is.na(first_hypo), list(hypo.sd=sd(Yr_0_bio_t_chol, na.rm=T)) ]
kndp[ is.na(first_hypo), list(nonHypo.sd=sd(Yr_0_bio_t_chol, na.rm=T)) ]
kndp [ , list(total.mean=mean(Yr_0_bio_t_chol, na.rm=T), total.sd=sd(Yr_0_bio_t_chol, na.rm=T)) ]
t.test(kndp[ !is.na(first_hypo), Yr_0_bio_hdl_c ], kndp[ is.na(first_hypo), Yr_0_bio_hdl_c ])					#HDL 콜레스테롤
kndp[ !is.na(first_hypo), list(hypo.sd=sd(Yr_0_bio_hdl_c, na.rm=T)) ]
kndp[ is.na(first_hypo), list(nonHypo.sd=sd(Yr_0_bio_hdl_c, na.rm=T)) ]
kndp [ , list(total.mean=mean(Yr_0_bio_hdl_c, na.rm=T), total.sd=sd(Yr_0_bio_hdl_c, na.rm=T)) ]
t.test(kndp[ !is.na(first_hypo), Yr_0_bio_ldl_c ], kndp[ is.na(first_hypo), Yr_0_bio_ldl_c ])					#LDL 콜레스테롤
kndp[ !is.na(first_hypo), list(hypo.sd=sd(Yr_0_bio_ldl_c, na.rm=T)) ]
kndp[ is.na(first_hypo), list(nonHypo.sd=sd(Yr_0_bio_ldl_c, na.rm=T)) ]
kndp [ , list(total.mean=mean(Yr_0_bio_ldl_c, na.rm=T), total.sd=sd(Yr_0_bio_ldl_c, na.rm=T)) ]
t.test(kndp[ !is.na(first_hypo), Yr_0_bio_BUN ], kndp[ is.na(first_hypo), Yr_0_bio_BUN ])					#BUN 혈중 요소 질소
kndp[ !is.na(first_hypo), list(hypo.sd=sd(Yr_0_bio_BUN, na.rm=T)) ]
kndp[ is.na(first_hypo), list(nonHypo.sd=sd(Yr_0_bio_BUN, na.rm=T)) ]
kndp [ , list(total.mean=mean(Yr_0_bio_BUN, na.rm=T), total.sd=sd(Yr_0_bio_BUN, na.rm=T)) ]
t.test(kndp[ !is.na(first_hypo), Yr_0_bio_s_cr ], kndp[ is.na(first_hypo), Yr_0_bio_s_cr ])	
kndp[ !is.na(first_hypo), list(hypo.sd=sd(Yr_0_bio_s_cr, na.rm=T)) ]
kndp[ is.na(first_hypo), list(nonHypo.sd=sd(Yr_0_bio_s_cr, na.rm=T)) ]
kndp [ , list(total.mean=mean(Yr_0_bio_s_cr, na.rm=T), total.sd=sd(Yr_0_bio_s_cr, na.rm=T)) ]
t.test(kndp[ !is.na(first_hypo), Yr_0_AST ], kndp[ is.na(first_hypo), Yr_0_AST ])											#AST
kndp[ !is.na(first_hypo), list(hypo.sd=sd(Yr_0_AST, na.rm=T)) ]
kndp[ is.na(first_hypo), list(nonHypo.sd=sd(Yr_0_AST, na.rm=T)) ]
kndp [ , list(total.mean=mean(Yr_0_AST, na.rm=T), total.sd=sd(Yr_0_AST, na.rm=T)) ]
t.test(kndp[ !is.na(first_hypo), Yr_0_AST ], kndp[ is.na(first_hypo), Yr_0_ALT ])											#ALT
kndp[ !is.na(first_hypo), list(hypo.sd=sd(Yr_0_ALT, na.rm=T)) ]
kndp[ is.na(first_hypo), list(nonHypo.sd=sd(Yr_0_ALT, na.rm=T)) ]
kndp [ , list(total.mean=mean(Yr_0_AST, na.rm=T), total.sd=sd(Yr_0_ALT, na.rm=T)) ]


#흡연, 음주 분석 용
fisher.12 = function(field){
	table.hypo = table(kndp[ !is.na(first_hypo), eval(field) ])								
	table.nonHypo = table(kndp[ is.na(first_hypo), eval(field) ])
	table.total = table(kndp[ , eval(field)])
	rst = fisher.test(matrix(c(table.hypo['1'], table.hypo['2'], table.nonHypo['1'], table.nonHypo['2']), nrow=2, byrow=T))
	print('hypo');print(table.hypo)
	print('nonHypo');print(table.nonHypo)
	print('total');print(table.total)
	print(rst)
}
#병력, 약제사용 분석 용
fisher.10 = function(field){
	table.hypo = table(kndp[ !is.na(first_hypo), eval(field) ])								
	table.nonHypo = table(kndp[ is.na(first_hypo), eval(field) ])
	table.total = table(kndp[ , eval(field)])
	rst = fisher.test(matrix(c( ifelse(is.na(table.hypo['1']), 0, table.hypo['1']),
												ifelse(is.na(table.hypo['0']), 0, table.hypo['0']) ,
												ifelse(is.na(table.nonHypo['1']), 0, table.nonHypo['1']) ,
												ifelse(is.na(table.nonHypo['0']), 0, table.nonHypo['0'])
											   ), nrow=2, byrow=T))
	print('hypo');print(table.hypo)
	print('nonHypo');print(table.nonHypo)
	print('total');print(table.total)
	print(rst)
}

fisher.12(quote(Smoking_history))		#과거 흡연 유무 1:smoker 2:never_smoker
fisher.12(quote(Smoking_status))		#현재 흡연 유무 1:smoker 2:ex
fisher.12(quote(Alcohol_history))		#과거 음주 유무 1:yes 2:never
fisher.12(quote(Alcohol_status))		#현재 음주 유무 1:drinker 2:ex

fisher.10(quote(ht_hx_yes))					#고혈압
fisher.10(quote(chol_hx_yes))				#고지혈증
fisher.10(quote(cardiov_disease))		#심혈관질환
fisher.10(quote(cerebrov_disease))	#뇌혈관 질환
fisher.10(quote(periv_disease))			#말초혈관질환
cnt.hypo; cnt.nonHypo
fisher.10(quote(retino_hx_yes))				#미세혈관질환(망막증)

fisher.10(quote(sulfon_yn))				#설폰요소제
fisher.10(quote(metformin_yn))		#metformin
fisher.10(quote(insulin_yn))				#insulin
fisher.10(quote(alpha_gi_yn))			#AGI
fisher.10(quote(tzd_yn))					#TZD
fisher.10(quote(dm_miscell_yn))	#기타 당뇨병약제
fisher.10(quote(aspirin_yn))			#aspirin
fisher.10(quote(silostarzon_yn))			#silostarzol
fisher.10(quote(clopidogrel_yn))		#clopidogrel
fisher.10(quote(antiplatlete_miscell_yn))		#기타 항혈전제
fisher.10(quote(ace_inhibit_yn))						#ACEI
fisher.10(quote(arb_yn))									#ARB
fisher.10(quote(beta_block_yn))									#베타 차단제
fisher.10(quote(calcium_block_yn))									#CCB
fisher.10(quote(diuretics_yn))									#이뇨제
fisher.10(quote(bp_drug_miscell_yn))									#기타 항혈압제
fisher.10(quote(statin_yn))									#statin
fisher.10(quote(fibrate_drug_yn))									#fibrate
fisher.10(quote(lipid_miscell_yn))									#기타 고지혈제


#
#HIRA
#
#의료비 비교(person-year)
hypo.cost = kndp[ !is.na(first_hypo), (hypo_bef_cost_filter + hypo_aft_cost_filter)/(hypo_bef_days + hypo_aft_days)*365 ]	#저혈당
mean(hypo.cost);sd(hypo.cost)
nonHypo.cost = kndp[ is.na(first_hypo), (hypo_bef_cost_filter + hypo_aft_cost_filter)/(hypo_bef_days + hypo_aft_days)*365 ]	#비저혈당
mean(nonHypo.cost);sd(nonHypo.cost)
t.test(hypo.cost, nonHypo.cost)
cost = kndp[ , (hypo_bef_cost_filter + hypo_aft_cost_filter)/(hypo_bef_days + hypo_aft_days)*365 ]
mean(cost);sd(cost)

#입내원
hypo.vscn.in = kndp[ !is.na(first_hypo), (hypo_bef_vscn_in_filter + hypo_aft_vscn_in_filter)/(hypo_bef_days + hypo_aft_days)*365 ]	#저혈당, 입원
mean(hypo.vscn.in); sd(hypo.vscn.in)
nonHypo.vscn.in = kndp[ is.na(first_hypo), (hypo_bef_vscn_in_filter + hypo_aft_vscn_in_filter)/(hypo_bef_days + hypo_aft_days)*365 ]	#비저혈당, 입원
mean(nonHypo.vscn.in); sd(nonHypo.vscn.in)
t.test(hypo.vscn.in, nonHypo.vscn.in)
vscn.in = kndp[ , (hypo_bef_vscn_in_filter + hypo_aft_vscn_in_filter)/(hypo_bef_days + hypo_aft_days)*365 ]	#전체 입원
mean(vscn.in); sd(vscn.in)
hypo.vscn.out = kndp[ !is.na(first_hypo), (hypo_bef_vscn_out_filter + hypo_aft_vscn_out_filter)/(hypo_bef_days + hypo_aft_days)*365 ]	#저혈당, 외래
mean(hypo.vscn.out); sd(hypo.vscn.out)
nonHypo.vscn.out = kndp[ is.na(first_hypo), (hypo_bef_vscn_out_filter + hypo_aft_vscn_out_filter)/(hypo_bef_days + hypo_aft_days)*365 ]	#비저혈당, 외래
mean(nonHypo.vscn.out); sd(nonHypo.vscn.out)
t.test(hypo.vscn.out, nonHypo.vscn.out)
vscn.out = kndp[ , (hypo_bef_vscn_out_filter + hypo_aft_vscn_out_filter)/(hypo_bef_days + hypo_aft_days)*365 ]		#외래
mean(vscn.out); sd(vscn.out)
hypo.vscn = kndp[ !is.na(first_hypo), (hypo_bef_vscn_in_filter + hypo_aft_vscn_in_filter + hypo_bef_vscn_out_filter + hypo_aft_vscn_out_filter)/(hypo_bef_days + hypo_aft_days)*365 ]	#저혈당, 입원+외래
mean(hypo.vscn); sd(hypo.vscn)
nonHypo.vscn = kndp[ is.na(first_hypo), (hypo_bef_vscn_in_filter + hypo_aft_vscn_in_filter + hypo_bef_vscn_out_filter + hypo_aft_vscn_out_filter)/(hypo_bef_days + hypo_aft_days)*365 ]	#비저혈당, 입원+외래
mean(nonHypo.vscn); sd(nonHypo.vscn)
t.test(hypo.vscn, nonHypo.vscn)
vscn = kndp[ , (hypo_bef_vscn_in_filter + hypo_aft_vscn_in_filter + hypo_bef_vscn_out_filter + hypo_aft_vscn_out_filter)/(hypo_bef_days + hypo_aft_days)*365 ]	#입원+외래
mean(vscn); sd(vscn)



#요양기간
hypo.recn.in = kndp[ !is.na(first_hypo), (hypo_bef_recn_in_filter + hypo_aft_recn_in_filter)/(hypo_bef_days + hypo_aft_days)*365 ]	#저혈당, 입원
mean(hypo.recn.in); sd(hypo.recn.in)
nonHypo.recn.in = kndp[ is.na(first_hypo), (hypo_bef_recn_in_filter + hypo_aft_recn_in_filter)/(hypo_bef_days + hypo_aft_days)*365 ]	#비저혈당, 입원
mean(nonHypo.recn.in); sd(nonHypo.recn.in)
t.test(hypo.recn.in, nonHypo.recn.in)
recn.in = kndp[ , (hypo_bef_recn_in_filter + hypo_aft_recn_in_filter)/(hypo_bef_days + hypo_aft_days)*365 ]	#전체 입원
mean(recn.in); sd(recn.in)
hypo.recn.out = kndp[ !is.na(first_hypo), (hypo_bef_recn_out_filter + hypo_aft_recn_out_filter)/(hypo_bef_days + hypo_aft_days)*365 ]	#저혈당, 외래
mean(hypo.recn.out); sd(hypo.recn.out)
nonHypo.recn.out = kndp[ is.na(first_hypo), (hypo_bef_recn_out_filter + hypo_aft_recn_out_filter)/(hypo_bef_days + hypo_aft_days)*365 ]	#비저혈당, 외래
mean(nonHypo.recn.out); sd(nonHypo.recn.out)
t.test(hypo.recn.out, nonHypo.recn.out)
recn.out = kndp[ , (hypo_bef_recn_out_filter + hypo_aft_recn_out_filter)/(hypo_bef_days + hypo_aft_days)*365 ]		#외래
mean(recn.out); sd(recn.out)
hypo.recn = kndp[ !is.na(first_hypo), (hypo_bef_recn_in_filter + hypo_aft_recn_in_filter + hypo_bef_recn_out_filter + hypo_aft_recn_out_filter)/(hypo_bef_days + hypo_aft_days)*365 ]	#저혈당, 입원+외래
mean(hypo.recn); sd(hypo.recn)
nonHypo.recn = kndp[ is.na(first_hypo), (hypo_bef_recn_in_filter + hypo_aft_recn_in_filter + hypo_bef_recn_out_filter + hypo_aft_recn_out_filter)/(hypo_bef_days + hypo_aft_days)*365 ]	#비저혈당, 입원+외래
mean(nonHypo.recn); sd(nonHypo.recn)
t.test(hypo.recn, nonHypo.recn)
recn = kndp[ , (hypo_bef_recn_in_filter + hypo_aft_recn_in_filter + hypo_bef_recn_out_filter + hypo_aft_recn_out_filter)/(hypo_bef_days + hypo_aft_days)*365 ]	#입원+외래
mean(recn); sd(recn)



#
#저혈당 환자 paired t-test
#
hypo.bef.cost = kndp[ !is.na(first_hypo), hypo_bef_cost_filter/hypo_bef_days*365 ]		#의료비
mean(hypo.bef.cost, na.rm=T); sd(hypo.bef.cost, na.rm=T)
hypo.aft.cost = kndp[ !is.na(first_hypo), hypo_aft_cost_filter/hypo_aft_days*365 ]
mean(hypo.aft.cost, na.rm=T); sd(hypo.aft.cost, na.rm=T)
t.test(hypo.bef.cost, hypo.aft.cost, paired=T)

hypo.bef.vscn.in = kndp[ !is.na(first_hypo), hypo_bef_vscn_in_filter/hypo_bef_days*365 ]	#입원 vscn
mean(hypo.bef.vscn.in, na.rm=T); sd(hypo.bef.vscn.in, na.rm=T)
hypo.aft.vscn.in = kndp[ !is.na(first_hypo), hypo_aft_vscn_in_filter/hypo_aft_days*365 ]
mean(hypo.aft.vscn.in, na.rm=T); sd(hypo.aft.vscn.in, na.rm=T)
t.test(hypo.bef.vscn.in, hypo.aft.vscn.in, paired=T)

hypo.bef.vscn.out = kndp[ !is.na(first_hypo), hypo_bef_vscn_out_filter/hypo_bef_days*365 ]	#외래 vscn
mean(hypo.bef.vscn.out, na.rm=T); sd(hypo.bef.vscn.out, na.rm=T)
hypo.aft.vscn.out = kndp[ !is.na(first_hypo), hypo_aft_vscn_out_filter/hypo_aft_days*365 ]
mean(hypo.aft.vscn.out, na.rm=T); sd(hypo.aft.vscn.out, na.rm=T)
t.test(hypo.bef.vscn.out, hypo.aft.vscn.out, paired=T)

hypo.bef.vscn = kndp[ !is.na(first_hypo), (hypo_bef_vscn_in_filter + hypo_bef_vscn_out_filter)/hypo_bef_days*365 ]	#입원+외래 vscn
mean(hypo.bef.vscn, na.rm=T); sd(hypo.bef.vscn, na.rm=T)
hypo.aft.vscn = kndp[ !is.na(first_hypo), (hypo_aft_vscn_in_filter  + hypo_aft_vscn_out_filter)/hypo_aft_days*365 ]
mean(hypo.aft.vscn, na.rm=T); sd(hypo.aft.vscn, na.rm=T)
t.test(hypo.bef.vscn, hypo.aft.vscn, paired=T)

hypo.bef.recn.in = kndp[ !is.na(first_hypo), hypo_bef_recn_in_filter/hypo_bef_days*365 ]	#입원 recn
mean(hypo.bef.recn.in, na.rm=T); sd(hypo.bef.recn.in, na.rm=T)
hypo.aft.recn.in = kndp[ !is.na(first_hypo), hypo_aft_recn_in_filter/hypo_aft_days*365 ]
mean(hypo.aft.recn.in, na.rm=T); sd(hypo.aft.recn.in, na.rm=T)
t.test(hypo.bef.recn.in, hypo.aft.recn.in, paired=T)

hypo.bef.recn.out = kndp[ !is.na(first_hypo), hypo_bef_recn_out_filter/hypo_bef_days*365 ]	#외래 recn
mean(hypo.bef.recn.out, na.rm=T); sd(hypo.bef.recn.out, na.rm=T)
hypo.aft.recn.out = kndp[ !is.na(first_hypo), hypo_aft_recn_out_filter/hypo_aft_days*365 ]
mean(hypo.aft.recn.out, na.rm=T); sd(hypo.aft.recn.out, na.rm=T)
t.test(hypo.bef.recn.out, hypo.aft.recn.out, paired=T)

hypo.bef.recn = kndp[ !is.na(first_hypo), (hypo_bef_recn_in_filter + hypo_bef_recn_out_filter)/hypo_bef_days*365 ]	#입원+외래 recn
mean(hypo.bef.recn, na.rm=T); sd(hypo.bef.recn, na.rm=T)
hypo.aft.recn = kndp[ !is.na(first_hypo), (hypo_aft_recn_in_filter  + hypo_aft_recn_out_filter)/hypo_aft_days*365 ]
mean(hypo.aft.recn, na.rm=T); sd(hypo.aft.recn, na.rm=T)
t.test(hypo.bef.recn, hypo.aft.recn, paired=T)



#multiple regression
kndp[ , Alcohol_history:=as.factor(Alcohol_history) ]
kndp[ , Alcohol_status:=as.factor(Alcohol_status) ]
kndp[ , chol_hx_yes:=as.factor(chol_hx_yes) ]
kndp[ , cardiov_disease:=as.factor(cardiov_disease) ]
kndp[ , dm_miscell_yn:=as.factor(dm_miscell_yn) ]
kndp[ , diuretics_yn:=as.factor(diuretics_yn) ]
kndp[ , bp_drug_miscell_yn:=as.factor(bp_drug_miscell_yn) ]
kndp[ , fu_years:=(hypo_bef_days + hypo_aft_days)/365 ]
kndp[ , cost:=(hypo_bef_cost_filter + hypo_aft_cost_filter)/fu_years ]
kndp[ , vscn:=(hypo_bef_vscn_in_filter + hypo_aft_vscn_in_filter + hypo_bef_vscn_out_filter + hypo_aft_vscn_out_filter)/fu_years ]
kndp[ , recn:=(hypo_bef_recn_in_filter + hypo_aft_recn_in_filter + hypo_bef_recn_out_filter + hypo_aft_recn_out_filter)/fu_years ]
kndp[ , is_hypo:=!is.na(first_hypo) ]
kndp[ , is_dead:=!is.na(death_date) ]

#추적기간, 나이, 성별, 당뇨기간, 의료비, 입내원기간, 요양기간, bmi, 이완기혈압, 총콜레스테롤, bun, creatinine, AST, ALT, 음주과거력
#고지혈증, 뇌혈관질환, 기타당뇨병약제, 이뇨제, 기타혈압약
## 현재 음주 상태 포함시 에러 발생(Alcohol_status)
##다음에 오류가 있습니다`contrasts<-`(`*tmp*`, value = contr.funs[1 + isOF[nn]]) : 
## contrasts는 오로지 2 또는 그 이상의 level들을 가진 요인들에만 적용할 수 있습니다

glm_rst = glm(is_hypo ~ fu_years + age2008 + gender + dm_duration + cost + vscn + recn + Yr_0_bio_bmi + Yr_0_bio_dia_bp + Yr_0_bio_t_chol + Yr_0_bio_BUN  +  Yr_0_bio_s_cr  + Yr_0_AST + Yr_0_ALT  + Alcohol_history + chol_hx_yes + cardiov_disease + dm_miscell_yn + diuretics_yn + bp_drug_miscell_yn, family='binomial', data=kndp)
summary(glm_rst)
confint(glm_rst)

#survival analysis


hypo.survfit = survfit(Surv(fu_years*365, is_dead) ~ is_hypo, data=kndp)
plot(hypo.survfit, lty=1:2, col=1:2, main='Survival curve', xlab='Survival time in days', ylab='Surviving(%)', fun='log')
legend('topright', c('non-hypoglycemia', 'hypoglycemia'), col=1:2, lty=1:2)
survdiff(Surv(fu_years*365, is_dead) ~ is_hypo, data=kndp)

#Cox proportional hazards model
cox.fit = coxph(Surv( fu_years*365/12, is_dead) ~ is_hypo + age2008 + dm_duration + recn + Yr_0_bio_s_cr + chol_hx_yes + dm_miscell_yn, data=kndp)
cox.fit = coxph(Surv( hypo_bef_days, is_hypo) ~ age2008 + gender + dm_duration + cost + vscn + recn + Yr_0_bio_bmi + Yr_0_bio_dia_bp + Yr_0_bio_t_chol + Yr_0_bio_BUN  +  Yr_0_bio_s_cr  + Yr_0_AST + Yr_0_ALT  + Alcohol_history + chol_hx_yes + cardiov_disease + dm_miscell_yn + diuretics_yn + bp_drug_miscell_yn , data=kndp)
summary(cox.fit)
confint(cox.fit)
cox.fit = coxph(Surv(hypo_bef_days) ~ age2008 + dm_duration + recn + Yr_0_bio_s_cr + chol_hx_yes + dm_miscell_yn, data=kndp)


plot(survfit(cox.fit))




