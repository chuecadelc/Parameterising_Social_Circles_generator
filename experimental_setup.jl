## Hamil and Gilbert Social Circles (2009) model replication ##

## DIRECTORIES ##

# pwd() Check which dir we're in
# change wd - cd("YOUR PATH")

## REQUIRED PACKAGES
# using Pkg ; Pkg.add("PACKAGE_NAME")  # If you need to download packages 
using Distributions

include("Social_Circles.jl")

# To calculate the general properties -from NetGenerators repo
include("github.com/chuecadelc/NetGenerators/Network_properties.jl") 

## MODEL PARAMS ##

# Torus dimensions
rows = 315
cols = 315

# Network size
population = 1000

# Distribution of social reaches (percentage)
proportions = [(1,0),(1,0),(1,0),(0.75,0.25),(0.75,0.25),(0.70,0.20),(0.34,0.33)]

# Single social reach
single_sr = fill(30 ,1000)

# Poisson dist.
Poisson_sr = rand(Poisson(30), 1000)

# Random uniform 
mean_range = 10:50
Random_sr= rand(Uniform(10,50), 1000) 

# Combining all options 
social_distances = [single_sr,Poisson_sr,Random_sr,[15,30,0],[30,50,0],[30,40,50],[30,40,50]]

# number of repetions
iters = 30 

# combining proportions and social reaches # 
for (prop_pairs, sr) in zip(keys(proportions), keys(social_distances))
   for rep in 1:iters 
     sc = Social_Circles_Proportions(rows,cols,population,proportions[prop_pairs][1],proportions[prop_pairs][2],social_distances[sr])
     # replacing corresponding run number and experiment in dfs
     replace!(experiment_trials[!, :run_number], 0 => rep)  
     replace!(experiment_trials[!, :experiment], 0 => prop_pairs) 
     replace!(df_nodes[!, :run_number], 0 => rep)  
     replace!(df_nodes[!, :experiment], 0 => prop_pairs) 
     # node-level centralities calculations
     node_centrality_cal(sc)
     replace!(node_level_df[!, :experiment], 0 => prop_pairs) 
     replace!(node_level_df[!, :run_number], 0 => rep)  
     println("Rep $rep")
   end
 end

# combine expermental setup and structural properties
combined = hcat(experiment_trials, df_properties) 
# export df
CSV.write("social_circles_experiments.csv", combined) 
