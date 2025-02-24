# Addtion analysis  on the 

knowledge_on_cholera_cause %>% 
  group_by(potential_causes_of_cholera) %>% 
  summarise(n=n()) %>% 
  mutate(n=round(n/sum(n), digits = 3)*100)
