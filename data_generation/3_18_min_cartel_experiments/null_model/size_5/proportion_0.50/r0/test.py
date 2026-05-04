# cited_node_arr = ["15065318", "14883802", "14863985", "14946898", "15015838", "15325543", "15295258", "15263454", "15247612", "15232580", "15186501", "15145245", "15137874", "15133551", "15117676", "15072335", "14956395", "15002539", "14878479", "15348407", "15301220", "15305145", "15294733", "15245852", "15230003", "15230226", "15190348", "15155486", "15117872", "15095594", "15075423", "14986870", "15026922", "14862141", "15051621"]
cited_node_arr = ["15232579", "14709678", "15328016", "14694934", "14807927", "15354550", "15323231", "15282486", "15293948", "15202605", "15231826", "15191190", "15156679", "15136298", "15101069", "15075423", "15053116", "15015838", "14883978", "15335483", "15354490", "15295927", "15266118", "15262626", "15216092", "15230054", "15175750", "15147256", "15136405", "15111349", "15088501", "15020184", "14994666", "14885283", "14806557"]

cartel_pubs = set()
with open("./output/nodelist.csv", "r") as f:
    for line_no,line in enumerate(f):
        if line_no == 0:
            continue
        node_id,node_type,year,alpha,pa_weight,fit_weight,num_authors_weight,author_reputation_weight,fit_lag_duration,fit_peak_value,fit_peak_duration,in_degree,out_degree,assigned_out_degree,planted_nodes_line_number,generator_node_string,sampled_neighborhood_size,fully_random_citations,author_id,num_authors,initial_author_reputation,final_author_reputation,cartel_id = line.strip().split(",")
        if cartel_id == "1":
            cartel_pubs.add(node_id)

print(f"num cartel pubs: {len(cartel_pubs)}")
cartel_citation = 0
for cited_node in cited_node_arr:
    if cited_node in cartel_pubs:
        cartel_citation += 1
print(f"num cartel citations: {cartel_citation}")
