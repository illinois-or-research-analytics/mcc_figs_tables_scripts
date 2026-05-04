import random
import click


@click.command()
@click.option("--input-nodelist", required=True, type=click.Path(exists=True), help="Input nodelist with author ids")
@click.option("--num-members", required=True, type=int, help="Number of cartel members")
@click.option("--from-last-x-years", required=True, type=int, help="Cartel members are chosen from authors born in the last x years")
@click.option("--cartel", required=True, type=bool, help="whether cartel should exist or not")
@click.option("--control", required=True, type=bool, help="whether control should exist or not")
@click.option("--output-nodelist", required=True, type=click.Path(), help="Output nodelist with cartel ids")
def make_cartels(input_nodelist, num_members, from_last_x_years, cartel, control, output_nodelist):
    author_birth_year = {}
    author_initial_reputation_dict = {}
    last_year = -1
    with open(input_nodelist, "r") as f:
        for line_no,line in enumerate(f):
            if line_no == 0:
                continue
            node_id,node_type,year,alpha,pa_weight,fit_weight,num_authors_weight,author_reputation_weight,fit_lag_duration,fit_peak_value,fit_peak_duration,in_degree,out_degree,assigned_out_degree,planted_nodes_line_number,generator_node_string,sampled_neighborhood_size,fully_random_citations,author_id,num_authors,initial_author_reputation,final_author_reputation,cartel_id = line.strip().split(",")
            if author_id not in author_birth_year:
                author_birth_year[author_id] = int(year)
            if author_id not in author_initial_reputation_dict:
                author_initial_reputation_dict[author_id] = int(initial_author_reputation)

            author_birth_year[author_id] = min(int(year), author_birth_year[author_id])
            author_initial_reputation_dict[author_id] = min(int(initial_author_reputation), author_initial_reputation_dict[author_id])
            last_year = max(int(year), last_year)


    cartel_elligible_authors = []
    for author,birth_year in author_birth_year.items():
        if birth_year > last_year - from_last_x_years:
            cartel_elligible_authors.append(author)
    random.shuffle(cartel_elligible_authors)

    cartel_authors = set(cartel_elligible_authors[:num_members])
    # print(cartel_authors)
    control_authors = []
    for cartel_author in cartel_authors:
        control_candidates = []
        cartel_author_birth_year = author_birth_year[cartel_author]
        cartel_author_initial_reputation = author_initial_reputation_dict[cartel_author]
        for author_id in author_birth_year:
            if author_id not in cartel_authors and author_id not in control_authors:
                if author_birth_year[author_id] == cartel_author_birth_year and author_initial_reputation_dict[author_id] == cartel_author_initial_reputation:
                    control_candidates.append(author_id)
        random.shuffle(control_candidates)
        if len(control_candidates) > 0:
            control_authors.append(control_candidates[0])
    # print(control_authors)


    with open(input_nodelist, "r") as fr:
        with open(output_nodelist, "w") as fw:
            for line_no,line in enumerate(fr):
                if line_no == 0:
                    fw.write(line)
                else:
                    node_id,node_type,year,alpha,pa_weight,fit_weight,num_authors_weight,author_reputation_weight,fit_lag_duration,fit_peak_value,fit_peak_duration,in_degree,out_degree,assigned_out_degree,planted_nodes_line_number,generator_node_string,sampled_neighborhood_size,fully_random_citations,author_id,num_authors,initial_author_reputation,final_author_reputation,cartel_id = line.strip().split(",")
                    if cartel and author_id in cartel_authors:
                        fw.write(f"{node_id},{node_type},{year},{alpha},{pa_weight},{fit_weight},{num_authors_weight},{author_reputation_weight},{fit_lag_duration},{fit_peak_value},{fit_peak_duration},{in_degree},{out_degree},{assigned_out_degree},{planted_nodes_line_number},{generator_node_string},{sampled_neighborhood_size},{fully_random_citations},{author_id},{num_authors},{initial_author_reputation},{final_author_reputation},1\n")
                    elif control and author_id in control_authors:
                        fw.write(f"{node_id},{node_type},{year},{alpha},{pa_weight},{fit_weight},{num_authors_weight},{author_reputation_weight},{fit_lag_duration},{fit_peak_value},{fit_peak_duration},{in_degree},{out_degree},{assigned_out_degree},{planted_nodes_line_number},{generator_node_string},{sampled_neighborhood_size},{fully_random_citations},{author_id},{num_authors},{initial_author_reputation},{final_author_reputation},0\n")
                    else:
                        fw.write(line)

if __name__ == "__main__":
    make_cartels()
