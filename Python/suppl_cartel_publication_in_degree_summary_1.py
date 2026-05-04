# experiment,scoring_model,proportion,size,rep,author_id,in_degree
import numpy as np

data_dict = {}
with open("./output/comparable_authors.csv", "r") as f:
    for line_no,line in enumerate(f):
        if line_no == 0:
            continue
        experiment,scoring_model,proportion,size,rep,author_id,in_degree = line.strip().split(",")
        if experiment not in data_dict:
            data_dict[experiment] = {}
        if scoring_model not in data_dict[experiment]:
            data_dict[experiment][scoring_model] = {}
        if size not in data_dict[experiment][scoring_model]:
            data_dict[experiment][scoring_model][size] = []
        data_dict[experiment][scoring_model][size].append(int(in_degree))

print(f"experiment,scoring_model,size,min,q1,median,q3,90,99,max")
for experiment in data_dict:
    for size in ["size_5", "size_25", "size_125", "size_250"]:
        for scoring_model in ["control_model", "null_model", "phenotype_model", "background-agent"]:
            in_degree_arr = data_dict[experiment][scoring_model][size]
            current_min = np.min(in_degree_arr)
            current_median = np.median(in_degree_arr)
            current_max = np.max(in_degree_arr)
            current_q1 = np.percentile(in_degree_arr, 25)
            current_q3 = np.percentile(in_degree_arr, 75)
            current_90 = np.percentile(in_degree_arr, 90)
            current_99 = np.percentile(in_degree_arr, 99)
            scoring_model_text = "ctrl"
            if scoring_model == "null_model":
                scoring_model_text = "cartel-r"
            elif scoring_model == "phenotype_model":
                scoring_model_text = "cartel-p"
            elif scoring_model == "background-agent":
                scoring_model_text = "bg-agent"
            print(f"{experiment},{scoring_model_text},{size.replace('size_', '')},{current_min},{current_q1},{current_median},{current_q3},{current_90},{current_99},{current_max}")
