# 
# 
# 
tf <- tempfile()

this_url <- "https://www.cms.gov/files/zip/cspuf2019.zip"

download.file( this_url , tf , mode = 'wb' )

unzipped_files <- unzip( tf , exdir = tempdir() )

mcbs_csv <- grep( '\\.csv$' , unzipped_files , value = TRUE )

mcbs_df <- read.csv( mcbs_csv )

names( mcbs_df ) <- tolower( names( mcbs_df ) )

library(survey)

mcbs_design <-
	svrepdesign(
		weight = ~cspufwgt ,
		repweights = 'cspuf[0-9]+' ,
		mse = TRUE ,
		type = 'Fay' ,
		rho = 0.3 ,
		data = mcbs_df
	)
mcbs_design <-
	update(
		
		mcbs_design ,
		
		csp_age =
			factor( 
				csp_age , 
				levels = 1:3 , 
				labels = 
					c( 
						'01: younger than 65' ,
						'02: 65 to 74' ,
						'03: 75 or older'
					)
			) ,
 		
		two_or_more_chronic_conditions = as.numeric( csp_nchrncnd > 1 ) ,

		csp_sex = factor( csp_sex , labels = c( 'male' , 'female' ) )
	)
sum( weights( mcbs_design , "sampling" ) != 0 )

svyby( ~ one , ~ csp_age , mcbs_design , unwtd.count )
svytotal( ~ one , mcbs_design )

svyby( ~ one , ~ csp_age , mcbs_design , svytotal )
svymean( ~ pamtoop , mcbs_design )

svyby( ~ pamtoop , ~ csp_age , mcbs_design , svymean )
svymean( ~ csp_sex , mcbs_design )

svyby( ~ csp_sex , ~ csp_age , mcbs_design , svymean )
svytotal( ~ pamtoop , mcbs_design )

svyby( ~ pamtoop , ~ csp_age , mcbs_design , svytotal )
svytotal( ~ csp_sex , mcbs_design )

svyby( ~ csp_sex , ~ csp_age , mcbs_design , svytotal )
svyquantile( ~ pamtoop , mcbs_design , 0.5 )

svyby( 
	~ pamtoop , 
	~ csp_age , 
	mcbs_design , 
	svyquantile , 
	0.5 ,
	ci = TRUE 
)
svyratio( 
	numerator = ~ pamtoop , 
	denominator = ~ pamttot , 
	mcbs_design 
)
sub_mcbs_design <- subset( mcbs_design , csp_income == 1 )
svymean( ~ pamtoop , sub_mcbs_design )
this_result <- svymean( ~ pamtoop , mcbs_design )

coef( this_result )
SE( this_result )
confint( this_result )
cv( this_result )

grouped_result <-
	svyby( 
		~ pamtoop , 
		~ csp_age , 
		mcbs_design , 
		svymean 
	)
	
coef( grouped_result )
SE( grouped_result )
confint( grouped_result )
cv( grouped_result )
degf( mcbs_design )
svyvar( ~ pamtoop , mcbs_design )
# SRS without replacement
svymean( ~ pamtoop , mcbs_design , deff = TRUE )

# SRS with replacement
svymean( ~ pamtoop , mcbs_design , deff = "replace" )
svyciprop( ~ two_or_more_chronic_conditions , mcbs_design ,
	method = "likelihood" )
svyttest( pamtoop ~ two_or_more_chronic_conditions , mcbs_design )
svychisq( 
	~ two_or_more_chronic_conditions + csp_sex , 
	mcbs_design 
)
glm_result <- 
	svyglm( 
		pamtoop ~ two_or_more_chronic_conditions + csp_sex , 
		mcbs_design 
	)

summary( glm_result )
library(srvyr)
mcbs_srvyr_design <- as_survey( mcbs_design )
mcbs_srvyr_design %>%
	summarize( mean = survey_mean( pamtoop ) )

mcbs_srvyr_design %>%
	group_by( csp_age ) %>%
	summarize( mean = survey_mean( pamtoop ) )
