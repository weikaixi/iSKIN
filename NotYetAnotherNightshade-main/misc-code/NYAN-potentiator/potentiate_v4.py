import os
import random
import math

import json
import joblib

from scipy import optimize as opt

model_directory = "extmodel5hk-models"

# Subsetted over 1000 datapoints; mean, std
reference_latent = [[-9.030216579403728, 3.8067878687929597], [5.903321242803125, 3.31483035126167], [5.741945569470524, 2.2442047253049577], [-7.504219630122185, 2.944938034809536], [0.7113706606845371, 3.200176975677866], [0.0869455133730662, 2.007161926472363], [1.0031348266396671, 2.8958185447969376], [13.813021309137344, 3.536570901445433], [-0.3384961927337572, 2.4391797772546306], [-3.392145679567475, 4.744317792449568], [13.683596758842468, 4.462321556181038], [0.34835000996198506, 3.074722064441879], [-0.013849202822893859, 2.3240747977521257], [-0.05667440175963566, 1.8937014331914657], [-0.9329220627909526, 3.277361804885961], [0.12599334719311445, 2.975860481916997], [-12.7798236079216, 3.553179087687643], [-1.0277703718543052, 3.6058957265805005], [-0.08075851694028825, 3.117858871300993], [-0.29033934938488526, 2.6310368456319955], [5.5576550832188225, 3.9554738427338103], [-0.18877321549260523, 2.1447434583014737], [-0.0992717707240954, 3.402423393892085], [0.33744355064909903, 3.1940918304361676], [0.25304281747853385, 3.0445507492398014], [-0.07195132426265627, 3.725154722477028], [0.1246961822900921, 2.0258538860293966], [-0.26221748151257634, 2.6610105232267194], [-0.04244814306672197, 3.109856019135291], [0.10933541307342239, 2.6158285107719705], [0.3931343628871837, 2.704682268225854], [0.37657008616998794, 3.1032590885087954], [0.33271945553086696, 2.9151788193536494], [-0.8788709844776895, 3.3785061711773765], [0.1625761494191829, 2.4774209893093677], [5.150338560955599, 3.283077578463169], [-0.304020764964167, 3.7148231504710636], [-0.11874300170037895, 3.0366012029156138], [-1.8786124333362095, 4.151159042882305], [-0.1440800912794657, 3.143621296992027], [-0.18170330851897598, 2.532243202282559], [5.643469664523844, 3.6548058605690463], [0.29436661452800034, 2.785457037994635], [2.998482128412463, 3.436489320525036], [-8.978382820740341, 3.8306500392541705], [0.4621298229042441, 3.3364769285574054], [-0.34111577708041296, 2.976356639675384], [5.671247780827805, 2.8885517118296162], [0.3974612182374112, 3.8649907786132163], [7.133312884990126, 3.212575691667255], [1.5152330240570009, 4.660726364790511], [-0.2770991715493146, 2.7357051607392586], [0.2710423171192524, 2.862415127253095], [-0.20929393685190006, 3.107516550897881], [0.8704352819975466, 2.567999720092215], [-0.2879636835528072, 2.8075992269716465], [-7.718307558938861, 4.050398846737011], [-5.286300101754488, 4.754560657191099], [-0.5715242183853989, 2.5831495929990145], [0.19252577164239482, 2.478831492798683], [11.329014422118664, 2.9603233047576856], [-0.3373505460328306, 3.473580425752299], [-0.09640505857381504, 2.115845154826509], [0.4047547414568253, 3.040825998312148]]

latent = [x[0] + random.gauss(0, x[1]) for x in reference_latent]

preferences = json.loads(open("model_prefs.json").read())

scale_factor = float()

# Load all models into memory
for model_pref in preferences:

    model_name = model_pref["model"]
    type = model_pref["type"]

    model_pref["clf"] = joblib.load(model_directory + "/" + model_name + ".joblib")

    if type == "classification":

        for label_weight in model_pref["label_weights"]:
            scale_factor += abs(label_weight)

    elif type == "regression":
        scale_factor += abs(model_pref["weight"])

def score (latent):

    score = float()

    for model_pref in preferences:

        model_name = model_pref["model"]
        type = model_pref["type"]

        if type == "classification":

            result = model_pref["clf"].predict_proba([latent])[0]

            for j in range(len(result)):

                score += result[j] * model_pref["label_weights"][j] / scale_factor

        elif type == "regression":

            result = model_pref["clf"].predict([latent])[0]
            score += result * model_pref["weight"] / scale_factor

    return -score

def log (x, f, context):
    print(f, context)
    #print("Convergence:", convergence)

bound_deviation = 10
bounds = [(x[0] - x[1] * bound_deviation, x[0] + x[1] * bound_deviation) for x in reference_latent]

print("Running DUAL ANNEALING")

optimised = opt.dual_annealing(score, bounds, callback=log)

latent = optimised.x.tolist()

print("\nFinal results")
print("===============")

for model_pref in preferences:

    model_name = model_pref["model"]
    type = model_pref["type"]

    if type == "classification":

        result = model_pref["clf"].predict_proba([latent])[0]
        print(model_name, result)

    elif type == "regression":

        result = model_pref["clf"].predict([latent])[0]
        print(model_name, result)

print("===============")

open("latents.json", "w+").write(json.dumps(latent))
