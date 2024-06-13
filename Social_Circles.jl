## Hamil and Gilbert Social Circles (2009) model replication ##

## DIRECTORIES ##

# pwd() Check which dir we're in
# change wd - cd("YOUR PATH")

## REQUIRED PACKAGES
# using Pkg ; Pkg.add("PACKAGE_NAME")  # If you need to download packages 
using Graphs, Random, DataFrames, StatsBase, GraphPlot, Statistics, Inequality, CSV, OrderedCollections


## EXPERIMENTAL SETUP DF ##

experiment_trials = DataFrame(
    run_number = Any[],
    experiment = Any[],
    nodes = Int64[],
    edges = Int64[],
    social_dist = Any[],
    prop_green =Int64[],
    prop_blue = Int64[],
    prop_purple = Int64[],
)

## GENERAL NODE LEVEL PROPERTIES DF ## 

df_nodes =  DataFrame(
    run_number = Any[],
    experiment = Any[],
    node_id = Any[],
    social_distance= Any[]
)

## node-level centrality measures ##

node_level_df = DataFrame()

# Eucledian distance between points so you can make an edge ##

function euclidean_distance_cal(pair1, pair2,rows,cols)
    x1, y1 = pair1
    x2, y2 = pair2

    # wrapping the torus around
    if abs(x1 - x2) > cols/2
        
        min_x_value = min(x1,x2)
    
        # adds length of rows to the smaller value of the two coordinates
        x1 += cols * (x1 == min_x_value)
        x2 += cols * (x2 == min_x_value)
    end

    # we also need to check the same for y dimension and add the length of col/row to lower value
    if abs(y1 - y2) > rows/2

        min_y_value = min(y1,y2)
        # adds length of rows to the smaller value of the two coordinates
        y1 += rows * (y1 == min_y_value)
        y2 += rows * (y2 == min_y_value)
       
    end

    return sqrt((x2 - x1)^2 + (y2 - y1)^2)

end



############################################

function Social_Circles(rows,cols,nodes_req,green,blue,social_reach)

    graph = Graph(nodes_req) 

    # create original matrix w/ specified dimensions
    data = zeros(rows,cols)

    # sample 1000 observations - matching the population from H&G(2009) model
    selected_nodes = sample(1:length(data),nodes_req, replace=false)
   
    # replace at index[i] for 1 - placing our agents 
    data[selected_nodes] .= 1

    # assigning distances to our set of nodes that can create edges
    nodes_dict = Dict{Int, Any}()

    num_nodes = length(selected_nodes)
    num_green = Int(round(green * num_nodes))
    num_blue = Int(round(blue * num_nodes))

    # asign the social reach to nodes
    for node in 1:length(selected_nodes)

        if node <= num_green

            if length(social_reach) > 3 

                social_dist_index = sample(1:length(social_reach),1, replace=false)

                nodes_dict[node] = social_reach[social_dist_index][1]

            else
                nodes_dict[node] = social_reach[1]
            end
            
        elseif node <= num_blue + num_green

            nodes_dict[node] = social_reach[2]
        else
            nodes_dict[node] = social_reach[3]
        end
    end

    # Coordinates for the nodes so we can use them for eucledian dist calculation
    pairs = []
    for each in selected_nodes
        row_pos = Int64(ceil(each/ rows))
        col_pos = mod(each, cols)
        push!(pairs, (each, (row_pos, col_pos)))
    end

    # only nodes w/ same distance can pair up 
    for (node1, s_reach1) in nodes_dict

        for (node2, s_reach2) in nodes_dict

            EUC_dist = euclidean_distance_cal(pairs[node1][2],pairs[node2][2],rows,cols)
           
            # only create an edge if their distances overlap; it doesn't exist or it's not a self-loop
            if node1 != node2 && EUC_dist <= min(s_reach1,s_reach2) && !has_edge(graph, node1, node2)
                add_edge!(graph, node1, node2)
            end
        end
    end

    ## Data saving step
    num_purple = num_nodes - (num_green+num_blue)
    
    push!(experiment_trials, (0,0,nv(graph),ne(graph),social_reach,num_green,num_blue,num_purple))
    centrality_calc(graph)

    for (key,value) in nodes_dict
        push!(df_nodes,(run_number= 0,experiment = 0, node_id= key, social_distance = value))
    end
    
    return graph 

end

# outputing the node-level measures 
function node_centrality_cal(network)

    experiment = fill(0,1000) # change to 2000 for density experiments
    run_number = fill(0,1000)
    node_id = collect(vertices(network))
    node_deg = degree(network)
    node_close = betweenness_centrality(network)
    node_betw = closeness_centrality(network)
    node_eig = eigenvector_centrality(network)
    local_clust = local_clustering_coefficient(network)

    col_to_row = hcat(experiment,run_number, node_id, node_deg, node_close, node_betw, node_eig, local_clust)

    col_names = ["experiment", "run_number","node_id", "node_deg", "node_close", "node_betw", "node_eig", "local_clust"]

    internal_df = DataFrame(col_to_row, col_names)

    append!(node_level_df,  internal_df)

    return node_level_df

end

