# Decision-Tree-and-Regression
Qualtrics has categorized their clients into five main revenue brackets based on the revenue they generate. An initial time series analysis of Qualtrics revenues shows that 70% of its revenue is generated by the top bracket companies. I tried to implement models that will predict whether a company belongs to this bracket or not by using their responses to Qualtrics marketing campaign and their behavior on Qualtrics platform. Initial exploratory analysis shows that high revenue generated for Qualtrics is no way co-related to the high number of projects created by companies on the platform. Hence, this task was much more complicated for the models and after multiple iterations two models presented promising results.
I applied my newly attained skills from the Business Analytics with R class to do the following analysis. Association rules analysis was done to establish relationships between subscription of various product offerings. Clustering Analysis on Support data provided a helpful insight that the support team was not prioritizing support requests appropriately. Clustering provided the insight that there is a group of clients who are raising many complaints, and their complaints are being resolved quickly. At the same time, there are groups who are not raising as many issues and still their complaints take a lot of time to resolve. Decision tree analysis achieved a high accuracy rate when applied to marketing data as it accurately (98%) categorized clients in revenue brackets 5 and 6 (top earning platinum clients) from lower revenue brackets (lead clients). Logistic regression when applied also helped to differentiate between platinum and lead clients when implemented on platform usage data.
