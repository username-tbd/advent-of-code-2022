table_lines = [
        "drop table if exists problem_01;",
        "create table problem_01 (row_num int, item_cals int);",
        "insert into problem_01 values"
        ]
table_text = "\n".join(table_lines)

def coalesce(l):
    return "null" if l == "" else l

with open("../inputs/input-01.txt") as f:
    input_lines = [l.rstrip("\n") for l in f.readlines()]

insert_lines = [f"({n}, {coalesce(l)})" for n, l in enumerate(input_lines)]
insert_text = ",\n".join(insert_lines)

query_text = table_text + "\n" + insert_text + ";"
with open("01-load.sql", "w") as f:
    f.writelines(query_text)
