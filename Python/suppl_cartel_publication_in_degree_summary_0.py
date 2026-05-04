import numpy as np
import random
from pathlib import Path

size_arr = ["size_125", "size_25", "size_250", "size_5"]
proportion_arr = ["proportion_0.50"]
rep_arr = ["r0", "r1", "r2"]
score_arr = ["null_model", "phenotype_model", "control_model"]
experiment_arr = ["3_18_min_cartel_experiments", "3_22_min_cartel_experiments"]
prefix="/projects/illinois/eng/cs/chackoge/illinoiscomputes/abm/v6/abm_outputs/" #3_18_min_cartel_experiments/"#null_model/size_5/r0/output"

def build_node_dict(input_nodelist):
    node_dict = {}
    nodelist_header_dict = {}
    with open(input_nodelist, "r") as f:
        for line_no,line in enumerate(f):
            line_arr = line.strip().split(",")
            if line_no == 0:
                for header_index,header_name in enumerate(line_arr):
                    nodelist_header_dict[header_name] = header_index
            else:
                current_node_id = line_arr[nodelist_header_dict["node_id"]]
                node_dict[current_node_id] = {}
                for header_name,header_index in nodelist_header_dict.items():
                    if header_name != "node_id":
                        node_dict[current_node_id][header_name] = line_arr[header_index]
    return node_dict

def parse_errors_path(errors_path):
    with open(errors_path, "r") as f:
        for line in f:
                # Elapsed (wall clock) time (h:mm:ss or m:ss): 8:15.12
            if "Elapsed" in line:
                time_string = line.strip().split("Elapsed (wall clock) time (h:mm:ss or m:ss): ")[1]
                time_arr = time_string.split(":")
                if len(time_arr) == 2:
                    m = float(time_arr[0])
                    s = float(time_arr[1])
                    return round(m * 60 + s)
                elif len(time_arr) == 3:
                    h = float(time_arr[0])
                    m = float(time_arr[1])
                    s = float(time_arr[2])
                    return round(h * 3600 + m * 60 + s)
    return runtime

def build_adjacency_dicts(input_edgelist, node_dict):
    forward_adjacency_list = {}
    backward_adjacency_list = {}
    with open(input_edgelist, "r") as f:
        for line_no,line in enumerate(f):
            if line_no == 0:
                continue
            source,target = line.strip().split(",")
            if source not in forward_adjacency_list:
                forward_adjacency_list[source] = []
            if target not in backward_adjacency_list:
                backward_adjacency_list[target] = []
            forward_adjacency_list[source].append(target)
            backward_adjacency_list[target].append(source)
    return forward_adjacency_list,backward_adjacency_list


with open("./output/comparable_authors.csv", "w") as f:
    f.write("experiment,scoring_model,proportion,size,rep,author_id,in_degree\n")
    for current_experiment in experiment_arr:
        for current_size in size_arr:
            for current_proportion in proportion_arr:
                for current_scoring in score_arr:
                    for current_rep in rep_arr:
                        current_nodelist = f"{prefix}/{current_experiment}/{current_scoring}/{current_size}/{current_proportion}/{current_rep}/output/nodelist.csv"
                        current_edgelist = f"{prefix}/{current_experiment}/{current_scoring}/{current_size}/{current_proportion}/{current_rep}/output/edgelist.csv"
                        current_node_dict = build_node_dict(current_nodelist)
                        for node in current_node_dict:
                            if current_node_dict[node]["cartel_id"] != "-1" and current_node_dict[node]["type"] == "agent":
                                current_author_id = current_node_dict[node]["author_id"]
                                current_in_degree = current_node_dict[node]["in_degree"]
                                f.write(f"{current_experiment},{current_scoring},{current_proportion},{current_size},{current_rep},{current_author_id},{current_in_degree}\n")
                            elif current_node_dict[node]["cartel_id"] == "-1" and current_node_dict[node]["type"] == "agent":
                                current_author_id = current_node_dict[node]["author_id"]
                                current_in_degree = current_node_dict[node]["in_degree"]
                                f.write(f"{current_experiment},background-agent,{current_proportion},{current_size},{current_rep},{current_author_id},{current_in_degree}\n")
