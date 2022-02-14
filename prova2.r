modelo = glm(formula = pap ~ instrucao_1 + instrucao_2 + tamanho + local_1 + local_2 + renda, family = binomial(link = "logit"), data = mqa)
summary(modelo)