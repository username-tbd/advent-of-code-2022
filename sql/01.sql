with windowed as (
  select item_cals,
    sum(case when item_cals is null then 1 else 0 end)
      over (order by row_num) as elf_id
  from problem_01
),

top_3 as (
  select elf_id,
    sum(item_cals) as elf_cals
  from windowed
  group by elf_id
  order by elf_cals desc
  limit 3
)

select max(elf_cals) as part1,
  sum(elf_cals) as part2
from top_3
