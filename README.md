# Coal and gas brownfield generation capacity automation

This project is part of the 
[china-re-pathways](https://github.com/east-winds/china-re-pathways) project.
This sub-project answers the question: what is the coal and gas generation 
capacity for China between 2020 and 2060? Since coal and gas cap are exogenous 
in the original model, this sub-project attempts to figure out a reasonable 
coal and gas capacity trajectory without using optimization. 

There are two forces that impacts coal and gas trajectory in this project. 
The first is 
natural retirement. Each power plants are assumed to have 40 years of life time 
before they are retired. The second is active retrofitting -- retrofitting 
existing unabated coal and gas power plants with carbon capture and sequestration 
technologies (CCS). The retrofit decisions are made based on both an overall 
2C emission trajectory in the power sector
and remaining emission in a power plants' lifetime. More specifically, we first
calculate emission gap for a specific year $t$:

$$emissionGap_t = currEmission_t - emissionTarget_t$$

More specifically, `currEmission` represents current CO2 emission from the 
power sector as a whole, accounting for natural retirement for the year. 

The `emissionTarget` represents the emission target of the 
whole power sector in a 2C scenario. 

Once the `emissionGap_t` is computed for the year, we then start retrofitting 
existing coal and gas power plants, ranked by the remaining CO2 emission 
in their lifetime, until the `emissionGap_t` is covered by the CO2 emission 
reduction from retrofitting. For example, a newly constructed coal power plant 
with high generation capacity and a long remaining lifetime will be retrofitted 
first because it has more remaining CO2 emission in its lifetime. 

This process is repeated for all snapshot years.

After running the model, the project will generate a file recording all 
retrofitting decisions in all the snapshot years. This file will then 
be used in the main project china-re-pathways. 

# Data Source:
1. [Coal](https://globalenergymonitor.org/projects/global-coal-plant-tracker/) 
and [gas](https://globalenergymonitor.org/projects/global-gas-plant-tracker/) 
power plant data are from the Global Energy Monitor. 

2. Emission trajectory is from 碳中和目标下的能源经济--转型路径与政策研究 by 张希良
et al. The paper can be accessed through [cnki](https://oversea.cnki.net/KCMS/detail/detail.aspx?dbcode=CJFD&dbname=CJFDAUTO&filename=GLSJ202201003&uniplatform=OVERSEAS_EN&v=_fG3sk79BwBKwfDbeV6jJscnzbv1b24KvaGLYg19AiMJUMzyAPLJ1DdQK5N9GIMx), but you 
can also find a copy in this repo under `literatures`. 



