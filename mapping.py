# -*- coding: utf-8 -*-
"""
Created on Wed Jul 26 16:57:51 2023

@author: mwk
"""

from autogluon.tabular import TabularPredictor
from osgeo import gdal
import os
import numpy as np
import rasterio
import pandas as pd


EV_path = r"G:\NEON\EV\clip_raster"
l8to_path = r"G:\GLAD\NEON_MetricsC"
map_path = r"G:\NEON\mapping"
in_l8to_metrics = ["dem","SVVI_avmin25","SVVI_min","nir_avmin25_LST","S1N_max",
              "slope","S1N_av75max","nir_av75max_S2N","red_av75max_RN",
              "aspect","nir_min","nir_max","S1S2_av75max","red_max_RN",
              "nir_av75max","red_avmin25_S2N","S1S2_median","nir_sd",
              "green_max_S2N","blue_avmin25","RN_av75max"]
ev_nmb = 0
site = 'abby'

parameters = []
print('metrics preparating')
for l8to_metrics in in_l8to_metrics:
    l8to_metrics_file = l8to_metrics +'.tif'
    l8to_sitePath = os.path.join(l8to_path,site,l8to_metrics_file)
    with rasterio.open(l8to_sitePath) as src:
        parameter = src.read(1)
        profile = src.profile   # 假设参数在第一个波段中
        parameters.append(parameter)

# 判断 ev_nmb 是否不等于 0
if ev_nmb != 0:
    ev_list = ['EV{}'.format(i) for i in range(1, ev_nmb+1)]
    for ev_metrics in ev_list:
        ev_metrics_file = ev_metrics +'.tif'
        EVto_sitePath = os.path.join(EV_path,site, ev_metrics_file)
        with rasterio.open(EVto_sitePath) as src:
            parameter = src.read(1)
            profile = src.profile   # 假设参数在第一个波段中
            parameters.append(parameter)
print('metrics preparated')


mapping = np.empty(parameter.shape, parameter.dtype)
model_path= 'AutogluonModels/' +  site + '_EV' + str(ev_nmb) + '_model'
predictor = TabularPredictor.load(model_path)

if ev_nmb == 0:
    columns_name = in_l8to_metrics
    map_data = pd.DataFrame(columns=columns_name, index=[0], dtype=np.float32)
else:
    columns_name = in_l8to_metrics + ev_list
    map_data = pd.DataFrame(columns=columns_name, index=[0], dtype=np.float32)

print('start mapping')
for row in range(0,mapping.shape[0]):
    print('start mapping row:',row)
    for col in range(0,mapping.shape[1]):
        for i in range(0,map_data.shape[1]):
            map_data.iloc[0,i] = parameters[i][row,col]
        predicted_value = predictor.predict(map_data, model='WeightedEnsemble_L2')
        mapping[row, col] = predicted_value
    


print('mapping completed')
map_fileName = site + 'tif'
with rasterio.open(map_fileName, 'w', **profile) as dst:
    dst.write(mapping, 1)
