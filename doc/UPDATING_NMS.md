# Getting new language files when NMS updated

1. Run "Unpack Game Files" in NMS modding station
2. Grab language files:
   ```
   %NMS_UNPACKED_ROOT%/LANGUAGE/NMS_\w+_ENGLISH.MBIN
   ```
3. copy lang files:
   ```
   NMS_LOC6_ENGLISH.EXML
   NMS_LOC5_ENGLISH.EXML
   NMS_LOC4_ENGLISH.EXML
   NMS_LOC1_ENGLISH.EXML
   NMS_UPDATE3_ENGLISH.EXML
   ```
4. copy product file: %NMS_UNPACKED_ROOT%/METADATA/REALITY/TABLES/NMS_REALITY_GCPRODUCTTABLE.MBIN
5. copy substance file: %NMS_UNPACKED_ROOT%/METADATA/REALITY/TABLES/NMS_REALITY_GCSUBSTANCETABLE.MBIN
6. Run setup functions with 