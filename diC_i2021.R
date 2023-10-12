library(tidyverse)
library("RColorBrewer")
library(reshape2)

#base functions und Tafeln (1997)
#Alter geht von 15 bis 69; x = 1 entspricht dem Alter 15, x = 55 entspricht dem Alter 69.
l_x <- c(1e+05,0,0,0,0,0,0,0) #Aktiven,, Inval 1J-6J, SterbeWS 
#Tafeln
#SterbeWS Aktiver M & W & D (x bzw. y bzw. d)
q_xya <- tibble(alter = 15:69,
               q_x   = c(0.000417,0.000557,0.000709,0.000850,0.000953,0.001012,0.001022,0.001004,0.000963,0.000911,0.000856,0.000808,0.000772,0.000752,0.000745,0.000752,0.000768,0.000791,0.000820,0.000855,0.000895,0.000945,0.001005,0.001083,0.001181,0.001301,0.001447,0.001623,0.001833,0.002082,0.002364,0.002669,0.002983,0.003302,0.003630,0.003981,0.004371,0.004812,0.005308,0.005857,0.006460,0.007117,0.007831,0.008604,0.009454,0.010404,0.011504,0.012818,0.014429,0.016415,0.018832,0.021704,0.025016,0.028738,0.032822),
               q_y   = c(0.000228,0.000271,0.000310,0.000324,0.000330,0.000328,0.000322,0.000314,0.000304,0.000297,0.000293,0.000292,0.000292,0.000296,0.000302,0.000311,0.000327,0.000351,0.000386,0.000433,0.000490,0.000555,0.000624,0.000701,0.000783,0.000872,0.000972,0.001084,0.001213,0.001359,0.001524,0.001706,0.001903,0.002109,0.002324,0.002546,0.002782,0.003035,0.003306,0.003593,0.003898,0.004228,0.004585,0.004974,0.005402,0.005884,0.006449,0.007126,0.007935,0.008898,0.010025,0.011323,0.012797,0.014460,0.016332),
               q_d = (q_x+q_y)/2
               ) 
#InvalWS Aktiver M & W & D (x bzw. y bzw. d) (DAV 1997 I)
i_xy <- tibble(alter = 15:69, 
              i_x = c(0.0007490,0.0010288,0.0013064,0.0015749,0.0018206,0.0020282,0.0021829,0.0022678,0.0022807,0.0022807,0.0022807,0.0022807,0.0022807,0.0022807,0.0022807,0.0022807,0.0022807,0.0022807,0.0022807,0.0022807,0.0023012,0.0024604,0.0026587,0.0028520,0.0030383,0.0032306,0.0034725,0.0037716,0.0041007,0.0044404,0.0047767,0.0051541,0.0056249,0.0062273,0.0070534,0.0081259,0.0095007,0.0112013,0.0132062,0.0155535,0.0182793,0.0213377,0.0246920,0.0282059,0.0317913,0.0353828,0.0403322,0.0454595,0.0510343,0.0570642,0.0635517,0.0704939,0.0778820,0.0857010,0.0939290),
              i_y = c(0.0009245,0.0009363,0.0009464,0.0009568,0.0009704,0.0009898,0.0010111,0.0010388,0.0010775,0.0011190,0.0011571,0.0012028,0.0012566,0.0013239,0.0013958,0.0014808,0.0015882,0.0017230,0.0018848,0.0020537,0.0022212,0.0023908,0.0025592,0.0027505,0.0029852,0.0032737,0.0036134,0.0040140,0.0044868,0.0050157,0.0056054,0.0062894,0.0071055,0.0080782,0.0092114,0.0105028,0.0119495,0.0135635,0.0153174,0.0171766,0.0190882,0.0215467,0.0242570,0.0272921,0.0306883,0.0344857,0.0387280,0.0434633,0.0487439,0.0546268,0.0611742,0.0684529,0.0765351,0.0854985,0.0954258),
              i_d = (i_x+i_y)/2
              ) 
#SterbeWS M & W & D (x bzw. y bzw. d) im x-ten Jahr der Invalidisierung (DAV 1997 TI)
q_xyi <- tibble(alter   = 15:69, 
                q_xxi   = c(0.0020307,0.0021924,0.0024969,0.0029218,0.0034440,0.0040425,0.0046949,0.0053795,0.0062370,0.0073535,0.0086177,0.0099218,0.0111552,0.0123277,0.0135107,0.0146930,0.0158606,0.0170002,0.0181216,0.0192339,0.0203252,0.0213815,0.0223916,0.0233842,0.0243712,0.0253127,0.0261695,0.0269003,0.0275863,0.0282765,0.0288806,0.0293083,0.0294707,0.0294616,0.0294336,0.0293902,0.0293314,0.0292600,0.0290346,0.0285810,0.0279951,0.0273756,0.0268198,0.0262493,0.0255976,0.0249844,0.0245294,0.0243509,0.0243509,0.0243509,0.0243509,0.0243509,0.0243509,0.0243509,0.0243509),
                q_xm1xi = c(0.0000000,0.0019152,0.0021840,0.0025543,0.0030072,0.0035245,0.0040880,0.0046788,0.0054166,0.0063707,0.0074494,0.0085603,0.0096117,0.0106085,0.0116109,0.0126133,0.0136080,0.0145873,0.0155610,0.0165326,0.0174916,0.0184233,0.0193172,0.0201943,0.0210637,0.0218981,0.0226695,0.0233492,0.0239960,0.0246421,0.0252203,0.0256613,0.0258993,0.0260050,0.0260932,0.0261604,0.0262038,0.0262185,0.0260967,0.0257817,0.0253484,0.0248724,0.0244307,0.0239372,0.0233408,0.0227633,0.0223265,0.0221543,0.0221543,0.0221543,0.0221543,0.0221543,0.0221543,0.0221543,0.0221543),
                q_xm2xi = c(0.0000000,0.0000000,0.0018102,0.0021175,0.0024934,0.0029232,0.0033915,0.0038829,0.0044968,0.0052920,0.0061908,0.0071169,0.0079926,0.0088235,0.0096593,0.0104944,0.0113232,0.0121394,0.0129493,0.0137578,0.0145544,0.0153307,0.0160755,0.0168035,0.0175224,0.0182140,0.0188615,0.0194474,0.0200039,0.0205513,0.0210511,0.0214669,0.0217602,0.0219751,0.0221704,0.0223307,0.0224385,0.0224784,0.0224399,0.0223384,0.0221935,0.0220255,0.0218547,0.0216209,0.0213024,0.0209783,0.0207263,0.0206255,0.0206255,0.0206255,0.0206255,0.0206255,0.0206255,0.0206255,0.0206255),
                q_xm3xi = c(0.0000000,0.0000000,0.0000000,0.0015736,0.0018641,0.0021994,0.0025662,0.0029491,0.0034391,0.0040901,0.0048321,0.0055972,0.0063161,0.0069923,0.0076713,0.0083489,0.0090195,0.0096789,0.0103313,0.0109795,0.0116179,0.0122409,0.0128429,0.0134309,0.0140084,0.0145684,0.0151025,0.0156023,0.0160790,0.0165417,0.0169778,0.0173768,0.0177268,0.0180425,0.0183386,0.0186067,0.0188370,0.0190204,0.0191667,0.0192927,0.0194019,0.0194978,0.0195832,0.0196679,0.0197533,0.0198282,0.0198807,0.0199010,0.0199010,0.0199010,0.0199010,0.0199010,0.0199010,0.0199010,0.0199010),
                q_xm4xi = c(0.0000000,0.0000000,0.0000000,0.0000000,0.0011928,0.0014161,0.0016632,0.0019250,0.0022841,0.0027902,0.0033789,0.0039886,0.0045556,0.0050827,0.0056119,0.0061390,0.0066619,0.0071771,0.0076846,0.0081858,0.0086828,0.0091735,0.0096586,0.0101395,0.0106162,0.0110866,0.0115507,0.0120064,0.0124530,0.0128926,0.0133259,0.0137536,0.0141764,0.0145957,0.0150122,0.0154224,0.0158249,0.0162183,0.0166012,0.0169764,0.0173439,0.0176421,0.0180607,0.0184632,0.0189119,0.0193298,0.0196371,0.0197568,0.0197568,0.0197568,0.0197568,0.0197568,0.0197568,0.0197568,0.0197568),
                q_xi    = c(0.0000000,0.0000000,0.0000000,0.0000000,0.0000000,0.0006209,0.0007357,0.0008624,0.0010808,0.0014336,0.0018634,0.0023142,0.0027279,0.0031045,0.0034825,0.0038612,0.0042427,0.0046256,0.0050099,0.0053949,0.0057827,0.0061740,0.0065709,0.0069699,0.0073696,0.0077756,0.0081942,0.0086296,0.0090832,0.0095522,0.0100380,0.0105406,0.0110628,0.0116102,0.0121842,0.0127771,0.0133812,0.0139874,0.0146118,0.0152593,0.0159075,0.0165354,0.0171199,0.0177345,0.0183967,0.0190015,0.0194425,0.0196126,0.0196126,0.0196126,0.0196126,0.0196126,0.0196126,0.0196126,0.0196126),
                q_yyi   = c(0.0015161,0.0016368,0.0018641,0.0021813,0.0025712,0.0030180,0.0035051,0.0040162,0.0046564,0.0054899,0.0064337,0.0074073,0.0083282,0.0092035,0.0100867,0.0109694,0.0118411,0.0126919,0.0135291,0.0143595,0.0151742,0.0159628,0.0167169,0.0174580,0.0181948,0.0188977,0.0195374,0.0200830,0.0205951,0.0211104,0.0215614,0.0218807,0.0220020,0.0219952,0.0219743,0.0219419,0.0218980,0.0218447,0.0216764,0.0213378,0.0209003,0.0204378,0.0200229,0.0195970,0.0191104,0.0186526,0.0183130,0.0181797,0.0181797,0.0181797,0.0181797,0.0181797,0.0181797,0.0181797,0.0181797),
                q_ym1yi = c(0.0000000,0.0014298,0.0016305,0.0019070,0.0022451,0.0026313,0.0030520,0.0034931,0.0040439,0.0047562,0.0055615,0.0063909,0.0071758,0.0079200,0.0086684,0.0094167,0.0101593,0.0108905,0.0116174,0.0123428,0.0130587,0.0137543,0.0144217,0.0150765,0.0157256,0.0163485,0.0169244,0.0174318,0.0179147,0.0183971,0.0188288,0.0191580,0.0193357,0.0194146,0.0194804,0.0195306,0.0195630,0.0195740,0.0194831,0.0192479,0.0189244,0.0185690,0.0182393,0.0178708,0.0174256,0.0169944,0.0166683,0.0165398,0.0165398,0.0165398,0.0165398,0.0165398,0.0165398,0.0165398,0.0165398),
                q_ym2yi = c(0.0000000,0.0000000,0.0013514,0.0015809,0.0018615,0.0021824,0.0025320,0.0028989,0.0033572,0.0039509,0.0046219,0.0053133,0.0059670,0.0065874,0.0072114,0.0078348,0.0084536,0.0090629,0.0096676,0.0102712,0.0108659,0.0114455,0.0120015,0.0125450,0.0130817,0.0135981,0.0140815,0.0145189,0.0149343,0.0153430,0.0157161,0.0160266,0.0162455,0.0164060,0.0165518,0.0166715,0.0167519,0.0167817,0.0167530,0.0166772,0.0165690,0.0164436,0.0163161,0.0161415,0.0159038,0.0156618,0.0154737,0.0153984,0.0153984,0.0153984,0.0153984,0.0153984,0.0153984,0.0153984,0.0153984),
                q_ym3yi = c(0.0000000,0.0000000,0.0000000,0.0011748,0.0013917,0.0016420,0.0019159,0.0022017,0.0025675,0.0030536,0.0036075,0.0041787,0.0047154,0.0052203,0.0057272,0.0062331,0.0067337,0.0072260,0.0077131,0.0081970,0.0086736,0.0091387,0.0095881,0.0100271,0.0104583,0.0108764,0.0112751,0.0116482,0.0120041,0.0123496,0.0126751,0.0129730,0.0132343,0.0134700,0.0136911,0.0138912,0.0140632,0.0142001,0.0143093,0.0144034,0.0144849,0.0145565,0.0146203,0.0146835,0.0147472,0.0148032,0.0148424,0.0148575,0.0148575,0.0148575,0.0148575,0.0148575,0.0148575,0.0148575,0.0148575),
                q_ym4yi = c(0.0000000,0.0000000,0.0000000,0.0000000,0.0008905,0.0010572,0.0012417,0.0014372,0.0017052,0.0020831,0.0025226,0.0029778,0.0034011,0.0037946,0.0041897,0.0045832,0.0049736,0.0053582,0.0057371,0.0061113,0.0064823,0.0068487,0.0072108,0.0075699,0.0079258,0.0082769,0.0086234,0.0089636,0.0092971,0.0096252,0.0099487,0.0102680,0.0105837,0.0108967,0.0112077,0.0115139,0.0118144,0.0121081,0.0123940,0.0126741,0.0129485,0.0131711,0.0134836,0.0137841,0.0141191,0.0144311,0.0146605,0.0147499,0.0147499,0.0147499,0.0147499,0.0147499,0.0147499,0.0147499,0.0147499),
                q_yi    = c(0.0000000,0.0000000,0.0000000,0.0000000,0.0000000,0.0004635,0.0005493,0.0006438,0.0008069,0.0010703,0.0013912,0.0017277,0.0020366,0.0023177,0.0025999,0.0028827,0.0031675,0.0034533,0.0037402,0.0040277,0.0043172,0.0046093,0.0049056,0.0052035,0.0055019,0.0058050,0.0061176,0.0064426,0.0067813,0.0071314,0.0074941,0.0078693,0.0082592,0.0086678,0.0090964,0.0095390,0.0099900,0.0104426,0.0109088,0.0113922,0.0118761,0.0123449,0.0127812,0.0132401,0.0137345,0.0141860,0.0145152,0.0146422,0.0146422,0.0146422,0.0146422,0.0146422,0.0146422,0.0146422,0.0147499),
                q_ddi   = (q_xxi + q_yyi)/2,
                q_dm1di   = (q_xm1xi + q_ym1yi)/2,
                q_dm2di   = (q_xm2xi + q_ym2yi)/2,
                q_dm3di   = (q_xm3xi + q_ym3yi)/2,
                q_dm4di   = (q_xm4xi + q_ym4yi)/2,
                q_di   = (q_xi + q_yi)/2,
                ) 
r_xyi <- tibble(alter   = 15:69,
                r_xx    = c(0.0593504,0.0593851,0.0594823,0.0596333,0.0598305,0.0600637,0.0603255,0.0606070,0.0609912,0.0615400,0.0622105,0.0629605,0.0637480,0.0648122,0.0662048,0.0676015,0.0686773,0.0691084,0.0688194,0.0680367,0.0668848,0.0654881,0.0639710,0.0619181,0.0590458,0.0556838,0.0521594,0.0487995,0.0455566,0.0422076,0.0388246,0.0354776,0.0322368,0.0290401,0.0258386,0.0227215,0.0197778,0.0170986,0.0145282,0.0119768,0.0096342,0.0076881,0.0063274,0.0053428,0.0044499,0.0037210,0.0032293,0.0030491,0.0030491,0.0030491,0.0030491,0.0030491,0.0030491,0.0030491,0.0030491),
                r_xm1x  = c(0.0000000,0.0878873,0.0877132,0.0874426,0.0870903,0.0866721,0.0862036,0.0857004,0.0849891,0.0839603,0.0827254,0.0813946,0.0800788,0.0788222,0.0775431,0.0761600,0.0745933,0.0727614,0.0703725,0.0673622,0.0639894,0.0605139,0.0571962,0.0540104,0.0507824,0.0475517,0.0443544,0.0412284,0.0381480,0.0350873,0.0320817,0.0291672,0.0263799,0.0236626,0.0209821,0.0184130,0.0160303,0.0139094,0.0119857,0.0101728,0.0085109,0.0070387,0.0057963,0.0047165,0.0037230,0.0028451,0.0021134,0.0015552,0.0015552,0.0015552,0.0015552,0.0015552,0.0015552,0.0015552,0.0015552),
                r_xm2x  = c(0.0000000,0.0000000,0.1548972,0.1544824,0.1539438,0.1533040,0.1525872,0.1518168,0.1508498,0.1495177,0.1478096,0.1457131,0.1432168,0.1393225,0.1335180,0.1265228,0.1190544,0.1118328,0.1045745,0.0967776,0.0888291,0.0811165,0.0740262,0.0674914,0.0612156,0.0552391,0.0496053,0.0443557,0.0393530,0.0345209,0.0299982,0.0259230,0.0224318,0.0193814,0.0165716,0.0140563,0.0118905,0.0101279,0.0086870,0.0074310,0.0063342,0.0053700,0.0045104,0.0037196,0.0029886,0.0023324,0.0017680,0.0013110,0.0013110,0.0013110,0.0013110,0.0013110,0.0013110,0.0013110,0.0013110),
                r_xm3x  = c(0.0000000,0.0000000,0.0000000,0.1917457,0.1901559,0.1882682,0.1861541,0.1838822,0.1807862,0.1763716,0.1709683,0.1649048,0.1585114,0.1512497,0.1426966,0.1333507,0.1237097,0.1142726,0.1046717,0.0945683,0.0845084,0.0750380,0.0667032,0.0592885,0.0523008,0.0458667,0.0401132,0.0351669,0.0308815,0.0270096,0.0235328,0.0204292,0.0176793,0.0151654,0.0128248,0.0107188,0.0089100,0.0074596,0.0062988,0.0052979,0.0044261,0.0036557,0.0029560,0.0023011,0.0016939,0.0011444,0.0006637,0.0002625,0.0002625,0.0002625,0.0002625,0.0002625,0.0002625,0.0002625,0.0002625), 
                r_xm4x  = c(0.0000000,0.0000000,0.0000000,0.0000000,0.2186424,0.2153540,0.2116690,0.2077108,0.2020450,0.1938116,0.1839903,0.1735605,0.1635019,0.1535522,0.1430346,0.1323049,0.1217207,0.1116376,0.1018864,0.0922128,0.0828492,0.0740262,0.0659763,0.0585215,0.0514182,0.0447984,0.0387926,0.0335328,0.0288327,0.0244698,0.0205258,0.0170816,0.0142188,0.0117667,0.0095377,0.0075861,0.0059650,0.0047287,0.0037645,0.0029267,0.0022195,0.0016470,0.0012145,0.0008595,0.0005338,0.0002638,0.0000768,0.0000000,0.0000000,0.0000000,0.0000000,0.0000000,0.0000000,0.0000000,0.0000000),
                r_x     = c(0.0000000,0.0000000,0.0000000,0.0000000,0.0000000,0.1479184,0.1443681,0.1405533,0.1348957,0.1265664,0.1168056,0.1068532,0.0979513,0.0899973,0.0821515,0.0745450,0.0673084,0.0605751,0.0542021,0.0480508,0.0422593,0.0369641,0.0323020,0.0280888,0.0241278,0.0205387,0.0174434,0.0149607,0.0129635,0.0112227,0.0097097,0.0083939,0.0072461,0.0062159,0.0052795,0.0044458,0.0037264,0.0031314,0.0026370,0.0022052,0.0018251,0.0014872,0.0011812,0.0008956,0.0006297,0.0003890,0.0001775,0.0000000,0.0000000,0.0000000,0.0000000,0.0000000,0.0000000,0.0000000,0.0000000),  
                r_yy    = c(0.0557719,0.0558045,0.0558959,0.0560377,0.0562231,0.0564422,0.0566882,0.0569528,0.0573138,0.0578295,0.0584596,0.0591644,0.0599043,0.0609044,0.0622130,0.0635255,0.0645364,0.0649416,0.0646700,0.0639345,0.0628520,0.0615395,0.0601139,0.0581848,0.0554856,0.0523264,0.0490145,0.0458572,0.0428098,0.0396627,0.0364837,0.0333385,0.0302931,0.0272891,0.0242807,0.0213515,0.0185853,0.0160677,0.0136522,0.0112547,0.0090534,0.0072245,0.0059459,0.0050206,0.0041816,0.0034966,0.0030346,0.0028653,0.0028653,0.0028653,0.0028653,0.0028653,0.0028653,0.0028653,0.0028653),
                r_ym1y  = c(0.0000000,0.0825882,0.0824246,0.0821703,0.0818393,0.0814463,0.0810060,0.0805332,0.0798648,0.0788980,0.0777375,0.0764870,0.0752506,0.0740697,0.0728677,0.0715680,0.0700957,0.0683743,0.0661295,0.0633006,0.0601312,0.0568652,0.0537476,0.0507539,0.0477205,0.0446846,0.0416801,0.0387426,0.0358479,0.0329718,0.0301474,0.0274086,0.0247894,0.0222359,0.0197170,0.0173028,0.0150638,0.0130707,0.0112630,0.0095594,0.0079977,0.0066143,0.0054468,0.0044321,0.0034985,0.0026736,0.0019860,0.0014614,0.0014614,0.0014614,0.0014614,0.0014614,0.0014614,0.0014614,0.0014614),
                r_ym2y  = c(0.0000000,0.0000000,0.1455578,0.1451680,0.1446619,0.1440606,0.1433871,0.1426631,0.1417545,0.1405027,0.1388975,0.1369275,0.1345817,0.1309222,0.1254677,0.1188943,0.1118761,0.1050899,0.0982693,0.0909425,0.0834732,0.0762257,0.0695628,0.0634220,0.0575247,0.0519085,0.0466144,0.0416813,0.0369802,0.0324395,0.0281895,0.0243600,0.0210793,0.0182128,0.0155724,0.0132088,0.0111736,0.0095173,0.0081632,0.0069830,0.0059523,0.0050462,0.0042385,0.0034953,0.0028084,0.0021918,0.0016614,0.0012320,0.0012320,0.0012320,0.0012320,0.0012320,0.0012320,0.0012320,0.0012320),
                r_ym3y  = c(0.0000000,0.0000000,0.0000000,0.1801846,0.1786906,0.1769167,0.1749301,0.1727952,0.1698858,0.1657374,0.1606599,0.1549620,0.1489541,0.1421302,0.1340929,0.1253105,0.1162507,0.1073827,0.0983606,0.0888664,0.0794130,0.0705137,0.0626814,0.0557138,0.0491474,0.0431012,0.0376946,0.0330465,0.0290195,0.0253811,0.0221139,0.0191975,0.0166134,0.0142510,0.0120515,0.0100726,0.0083728,0.0070098,0.0059191,0.0049784,0.0041593,0.0034353,0.0027777,0.0021624,0.0015917,0.0010754,0.0006237,0.0002467,0.0002467,0.0002467,0.0002467,0.0002467,0.0002467,0.0002467,0.0002467),
                r_ym4y  = c(0.0000000,0.0000000,0.0000000,0.0000000,0.2054596,0.2023694,0.1989066,0.1951870,0.1898629,0.1821259,0.1728968,0.1630958,0.1536437,0.1442939,0.1344105,0.1243277,0.1143816,0.1049065,0.0957433,0.0866529,0.0778538,0.0695628,0.0619983,0.0549930,0.0483180,0.0420973,0.0364537,0.0315110,0.0270942,0.0229944,0.0192882,0.0160517,0.0133615,0.0110573,0.0089626,0.0071287,0.0056053,0.0044436,0.0035375,0.0027503,0.0020857,0.0015477,0.0011413,0.0008077,0.0005016,0.0002479,0.0000722,0.0000000,0.0000000,0.0000000,0.0000000,0.0000000,0.0000000,0.0000000,0.0000000),
                r_y     = c(0.0000000,0.0000000,0.0000000,0.0000000,0.0000000,0.1389998,0.1356635,0.1320787,0.1267623,0.1189352,0.1097629,0.1004105,0.0920454,0.0845710,0.0771982,0.0700504,0.0632501,0.0569228,0.0509341,0.0451537,0.0397113,0.0347354,0.0303544,0.0263952,0.0226730,0.0193004,0.0163916,0.0140586,0.0121819,0.0105461,0.0091243,0.0078878,0.0068092,0.0058411,0.0049612,0.0041778,0.0035017,0.0029426,0.0024780,0.0020723,0.0017151,0.0013975,0.0011099,0.0008416,0.0005917,0.0003655,0.0001668,0.0000000,0.0000000,0.0000000,0.0000000,0.0000000,0.0000000,0.0000000,0.0000000),
                r_dd   = (r_xx + r_yy)/2,
                r_dm1d   = (r_xm1x + r_ym1y)/2,
                r_dm2d   = (r_xm2x + r_ym2y)/2,
                r_dm3d   = (r_xm3x + r_ym3y)/2,
                r_dm4d   = (r_xm4x + r_ym4y)/2,
                r_d   = (r_x + r_y)/2,
                ) #ReaktivWS M & W (x bzw. y) im x-ten Jahr der Invalidisierung (DAV 1997 RI)


while(i < 2) {
  u <- c(q_xya$q_x[x], unlist(q_xyi[x, 9:14], use.names = FALSE),1.0000000) #spalte ganz rechts
  o <- c(0,unlist(r_xyi[x, 9:14], use.names = FALSE)[1:6],0) #spalte ganz links

  d <- matrix(0, 6, 6)
  diag(d) <- c(i_xy[[x,3]],1-u[2:6]-o[2:6]) #diagonale ??

  m <- matrix(0.0000000,6,2) #6x2 matrix ganz rechts

  #m[6,1] <- unlist(r_xyi[x, 2:7], use.names = FALSE)[6]hier nicht sicher wegen der Reaktivierung; was ist r_X? untere zeile ist 1-q_xi
  m[6,1] <- 1-u[7]-o[6]

  #verbinden
  d <- cbind(d,m)
  d <- rbind(d,u)
  d <- rbind(o,d)

  d[1,1] <- 1-d[2,1]-d[8,1]

  #l_1 <- d %*% l_x
  #h <- cbind(l_x, l_1)
  print(d)
  h <- cbind(h, (d %*% h[,i]))

 #h[8,i] < 1e+5
  
  x <- x + 1
  i <- i + 1
}


#p() berechnet die 1-Schritt-Übergangsmatrix p zum alter x.
p <- function(x, g = "u"){
  if(g == "w"){
    u <- c(q_xya$q_x[x], unlist(q_xyi[x, 8:13], use.names = FALSE),1.0000000) #spalte ganz rechts
    o <- c(0,unlist(r_xyi[x, 8:13], use.names = FALSE)[1:6],0) #spalte ganz links
    
    d <- matrix(0, 6, 6)
    diag(d) <- c(i_xy[[x,3]],1-u[2:6]-o[2:6]) #diagonale ??
    
    m <- matrix(0.0000000,6,2) #6x2 matrix ganz rechts
    
    #m[6,1] <- unlist(r_xyi[x, 2:7], use.names = FALSE)[6]hier nicht sicher wegen der Reaktivierung; was ist r_X? untere zeile ist 1-q_xi
    m[6,1] <- 1-u[7]-o[6]
    
    #verbinden
    d <- cbind(d,m)
    d <- rbind(d,u)
    d <- rbind(o,d)
    
    d[1,1] <- 1-d[2,1]-d[8,1]
    d <- t(d)
    return(d)
  } else if (g == "m"){
      u <- c(q_xya$q_y[x], unlist(q_xyi[x, 2:7], use.names = FALSE),1.0000000) #spalte ganz rechts
      o <- c(0,unlist(r_xyi[x, 2:7], use.names = FALSE)[1:6],0) #spalte ganz links
      
      d <- matrix(0, 6, 6)
      diag(d) <- c(i_xy[[x,2]],1-u[2:6]-o[2:6]) #diagonale ??
      
      m <- matrix(0.0000000,6,2) #6x2 matrix ganz rechts
      
      #m[6,1] <- unlist(r_xyi[x, 2:7], use.names = FALSE)[6]hier nicht sicher wegen der Reaktivierung; was ist r_X? untere zeile ist 1-q_xi
      m[6,1] <- 1-u[7]-o[6]
      
      #verbinden
      d <- cbind(d,m)
      d <- rbind(d,u)
      d <- rbind(o,d)
      
      d[1,1] <- 1-d[2,1]-d[8,1]
      d <- t(d)
      return(d)
    } else if(g == "u"){
      u <- c(q_xya$q_d[x], unlist(q_xyi[x, 14:19], use.names = FALSE),1.0000000) #spalte ganz rechts
      o <- c(0,unlist(r_xyi[x, 14:19], use.names = FALSE)[1:6],0) #spalte ganz links
      
      d <- matrix(0, 6, 6)
      diag(d) <- c(i_xy[[x,4]],1-u[2:6]-o[2:6]) #diagonale ??
      
      m <- matrix(0.0000000,6,2) #6x2 matrix ganz rechts
      
      m[6,1] <- 1-u[7]-o[6]
      
      #verbinden
      d <- cbind(d,m)
      d <- rbind(d,u)
      d <- rbind(o,d)
      
      d[1,1] <- 1-d[2,1]-d[8,1]
      d <- t(d)
      return(d)
    } else{
      print("für g muss m, w oder u eingegeben werden") 
    }
}

p4 <- function(x, g = "u"){
  d <- p(x, g)
  d[6,1] <- 0
  d[7,1] <- 0
  d[6,7] <- 1 - d[6,8]
  d[7,7] <- 1 - d[7,8]
  return(d)
}

p2 <- function(x, g = "u"){
  d <- p(x, g)
  d[4,1] <- 0
  d[5,1] <- 0
  d[6,1] <- 0
  d[7,1] <- 0
  d[4,7] <- 1 - d[4,8]
  d[5,7] <- 1 - d[5,8]
  d[6,7] <- 1 - d[6,8]
  d[7,7] <- 1 - d[7,8]
  d[4,5] <- d[4,7]
  d[5,6] <- d[5,7]
  d[4,7] <- 0
  d[5,7] <- 0
  return(d)
}
  
#pm() berechnet die m-Schritt-Übergangsmatrix pm zum alter x(g geschlecht, f funk)
pm <- function(x, m, f = 1, g ="u"){
  if(m == 0){
    a <- diag(nrow = 8)
    return(a)
  } else if(m == 1){
    if(f == 1){
      a <- p(x,g)
    } else {
      a <- p4(x,g)
    }
    return(a)
  } else if(m > 1){
    if(f == 1){
      print("p")
      a <- p(x,g)
      for(i in (x+1):(x+m-1)){
        a <- a %*% p(i,g) 
      }
    } else if( f == 2){
      print("p2")
      a <- p2(x,g)
      for(i in (x+1):(x+m-1)){
        a <- a %*% p2(i,g) 
        }
      }else if( f == 4){
          print("p4")
          a <- p4(x,g)
          for(i in (x+1):(x+m-1)){
          a <- a %*% p4(i,g) 
          }
        }
      }
    return(a)
}

#hm() berechnet die Populationsentwicklung nach m jahren zum Alter x (g Geschlecht, a)
hm <- function(x, m, a = 1, g="u"){
  mu <- c(1e5, rep(0,7))
  h <- mu
  if (a == 1){
    print("p")
    for(i in 1:m){
      h <- rbind(h, mu%*%pm(x,i,f=1,g))
    }
  } else if (a == 2){
    print("p2")
    for(i in 1:m){
      h <- rbind(h, mu%*%pm(x,i,f=2,g))
    }
  } else if ( a== 4){
    print("p4")
    for(i in 1:m){
      h <- rbind(h, mu%*%pm(x,i,f=4,g))
    }
  } else {
    print("wähle a = 1 für p4, a = 0 für p")
    
  }
  
  return(t(h))
}


###############################################################################################
#Plot
###############################################################################################
#Markov-Prozess p
x <- 1
m <- 54
mk <- NULL
mk <- c(1e5,0,0,0,0,0,0,0)
h <- mk
mk <- rbind(15:(length(nmk_a)+13),hm(x,m)) 
mk <- rbind(mk[1:2,], colSums(mk[3:8,]), mk[9,])
mkdata <- data.frame(t(mk))


#Markov-Prozess p2
x <- 1
m <- 54
mk2 <- NULL
mk2 <- c(1e5,0,0,0,0,0,0,0)
h2 <- mk2
mk2 <- rbind(15:(length(nmk_a)+13),hm(x,m,a=2)) 
mk2 <- rbind(mk2[1:2,], colSums(mk2[3:8,]), mk2[9,])
mk2data <- data.frame(t(mk2))


#Markov-Prozess p4
x <- 1
m <- 54
mk4 <- NULL
mk4 <- c(1e5,0,0,0,0,0,0,0)
h4 <- mk4
mk4 <- rbind(15:(length(nmk_a)+13),hm(x,m,a=4)) 
mk4 <- rbind(mk4[1:2,], colSums(mk4[3:8,]), mk4[9,])
mk4data <- data.frame(t(mk4))


#Kein Markov-Prozess
f1 <- 1
f2 <- 1
for (j in 0:54) {
  f1 <- c(f1,f1[j+1] * p(x+j)[1,1])
  f2 <- c(f2, pm(1,j+1)[1,1])
}

x <- 1
m <- 54
l_x <- c(1e5, rep(1e5,7))
nmk_a <- 1e5
nmk_i <- 1e5
for (i in 0:m){
  nmk_a <- c(nmk_a, nmk_a[i+1] * p(i+x)[1,1])
}
for (i in 0:m) {
  nmk_i <- c(nmk_i, nmk_i[i+1] * (1-i_xy[[x+i,4]])) 
}

nmk_d <- 1e5 - (nmk_i + nmk_a)
nmk_ges <- rbind(15:(length(nmk_a)+14),nmk_a, nmk_i, nmk_d) 
nmkdata <- data.frame(t(nmk_ges))

#Plott einer normalen Populationsentwicklung

gg <- NULL
gg <- ggplot() + geom_line(data = mkdata, aes(x=V1,y= o,colour= "Aktive(M)"), linetype=2, size =1.1)+
  geom_line(data = mkdata, aes(x=V1,y= V3,colour= "Invalide(M)"), linetype=2, size =1.1)+
  #geom_line(aes(y= X4,colour= "Verstorbene(M)"), linetype=2, size =1.1)+
  geom_line(data = nmkdata, aes(x=V1, y=nmk_a, colour = "Aktive(NM)"),linetype=5, size =1) +
  geom_line(data = nmkdata, aes(x=V1, y=nmk_i, colour = "Invalide(NM)"),linetype=5, size =1) +
  #geom_line(data = hdata, aes(x=V1, y=h_1, colour = "Verstorbene(NM)"),linetype=2, size =1) +
  xlab("Alter")+
  ylab("Anzahl")+
  scale_x_continuous(breaks=seq(0,70,5))+
  scale_color_manual(values= c("blue","darkorange","cyan4","red"), 
                     name="Legende:", 
                     labels=c("Aktive(M)","Aktive(NM)","Invalide(M)","Invalide(NM)")) +
  ggtitle("Markov vs Nicht-Markov")

ggz <- NULL
ggz <- ggplot() + geom_line(data = mkdata, aes(x=V1,y= o,colour= "Aktive(p)"), size =0.5)+
  geom_line(data = mkdata, aes(x=V1,y= V3,colour= "Invalide(p)"), size =0.5)+
  geom_line(data = mk2data, aes(x=V1, y=o, colour = "Aktive(p2)"), size =0.5) +
  geom_line(data = mk2data, aes(x=V1, y=V3, colour = "Invalide(p2)"), size =0.5) +
  geom_line(data = mk4data, aes(x=V1, y=o, colour = "Aktive(p4)"), size =0.5) +
  geom_line(data = mk4data, aes(x=V1, y=V3, colour = "Invalide(p4)"), size =0.5) +
  xlab("Alter")+
  ylab("Anzahl")+
  scale_x_continuous(breaks=seq(0,70,5))+
  scale_color_manual(values= c("blue","yellow", "magenta","cyan2","darkorange2","red"), 
                     name="Legende:", 
                     labels=c("Aktive(p)","Aktive(p2)","Aktive(p4)","Invalide(p)","Invalide(p2)","Invalide(p4)"))

#Plot der Tafeln 
#Inval
ggi <- NULL
ggi <- ggplot() + 
  geom_line(data = i_xy, aes(alter, i_d,col = "Ø"),size = 1) + 
  xlab("Alter") +
  ylab("InvalWS") +
  scale_color_manual(name="Legende:",
                     values= c("black"),
                     breaks =c("Ø"))


#SterbeWS
#SterbeWS-Aktiv

ggqa <- NULL
ggqa <- ggplot() + geom_line(data = q_xya, aes(alter, q_d, col = "Ø"),size = 1)+
  xlab("Alter") +
  ylab("SterbeWS") +
  scale_color_manual(name="Legende:",
                     values= c("black"),
                     breaks =c("Ø")) 

#ggqa_diff <- ggqi_diff + geom_line(data = q_xya, aes(x = alter, y= (q_x-q_y), col = "SterbeWS(A)"))

#SterbeWS-Inval
ggqi3 <- NULL
ggqi3 <- ggplot() + geom_line(data = q_xyi, aes(alter, q_ddi, col = "1. J"), size = 1) + 
  geom_line(data = q_xyi, aes(alter, q_dm1di, col = "2. J"), size = 1) + 
  geom_line(data = q_xyi, aes(alter, q_dm2di, col = "3. J"), size = 1) + 
  geom_line(data = q_xyi, aes(alter, q_dm3di, col = "4. J"), size = 1) + 
  geom_line(data = q_xyi, aes(alter, q_dm4di, col = "5. J"), size = 1) + 
  geom_line(data = q_xyi, aes(alter, q_di, col = "=>6. J"), size = 1) +
  xlab("Alter") +
  ylab("SterbeWS") +
  scale_color_manual(name="Legende:",
                     values= c(brewer.pal(n = 11, name = "Spectral")[1],brewer.pal(n = 11, name = "Spectral")[3],brewer.pal(n = 11, name = "Spectral")[5],
                                brewer.pal(n = 11, name = "Spectral")[8],brewer.pal(n = 11, name = "Spectral")[10],brewer.pal(n = 11, name = "Spectral")[11]),
                     breaks =c("1. J","2. J","3. J","4. J","5. J","=>6. J"))

#Reaktivierung
ggr <- NULL
ggr <- ggplot() + geom_line(data = r_xyi, aes(alter, r_dd, col = "1.J"),size =1) + 
  geom_line(data = r_xyi, aes(alter, r_dm1d, col = "2.J"),size =1) + 
  geom_line(data = r_xyi, aes(alter, r_dm2d, col = "3.J"),size =1) + 
  geom_line(data = r_xyi, aes(alter, r_dm3d, col = "4.J"),size =1) + 
  geom_line(data = r_xyi, aes(alter, r_dm4d, col = "5.J"),size =1) + 
  geom_line(data = r_xyi, aes(alter, r_d, col = "=>6.J"),size =1) +
  xlab("Alter") +
  ylab("ReaktWS") +
  scale_color_manual(name="Legende:",
                     values= c(brewer.pal(n = 11, name = "Spectral")[1],brewer.pal(n = 11, name = "Spectral")[3],brewer.pal(n = 11, name = "Spectral")[5],
                               brewer.pal(n = 11, name = "Spectral")[8],brewer.pal(n = 11, name = "Spectral")[10],brewer.pal(n = 11, name = "Spectral")[11]),
                     breaks =c("1.J","2.J","3.J","4.J","5.J","=>6.J"))


#Plott einer  Populationsentwicklung mit 4 zuständen und keiner Reaktivierung ab dem 4 Jahr
y <- rbind(1:(m+1),hm(x,m,1)) 
y_collapsed <- rbind(1:(m+1), y[2,], colSums(y[3:8,]), y[9,])
ydata <- data.frame(t(y_collapsed))
gg4 <- ggplot() + geom_line(data = ydata, col= "red", aes(X1, X2)) + geom_line(data = ydata, col= "blue", aes(X1, X3)) + geom_line(data = ydata, col= "green", aes(X1, X4)) 


gg4 <- ggplot() + geom_line(data = ydata, aes(X1, X2, col = "Aktive"),linetype=2, size =1.1) + 
  geom_line(data = ydata, aes(X1, X3, col = "Invalide"),linetype=2, size =1.1) + 
  geom_line(data = ydata, aes(X1, X4, col = "Verstorbene"),linetype=2, size =1.1) + 
  xlab("Alter") +
  ylab("Population") +
  scale_color_manual("",
                     values= c("green","red","black"),
                     breaks =c("Aktive","Invalide","Verstorbene"))

###############################################################################################
#Barwerte
#ä_x^a
vec <- c(0, rep(1,6),0)
#diskontfaktor
v <- 1/(1+0.0025)
e <- rep(0,8)
#eintrittsalter
x <- 10 
#dauer
l <- 57 - x

#Funktionen zur Prämienberechnung
#Nicht-Markov--------------------------------------------------

aak <- function(x,t){
  a <- 0
  lp <- 100000
  for (i in 1:dim(q_xya)[1]) {
    lp <- c(lp, lp[i]*(1-q_xya[[i,2]]- i_xy[[i,2]]))
  }
  for (i in 0:(t-1)) {
    a <- a + lp[x+i] * v^(x+i)
  }
  a <- a/(lp[x]*v^x)  
  return(a)
}

aik <- function(z,x,l){
  a <- 0
  lp <- 100000
  d <- x-z
  if(l==1){
    a <- 1
    return(a)
  } else if(l==0){
    a <- 0
    return(a)
  }
  if(d >= 5){d <- 5}
  for (i in 0:(l-1)) {
    a <- a + lp * v^(x+i)
    lp <- lp * (1-q_xyi[[x+i,d+2]]- r_xyi[[x+i,d+2]])
  }
  a <- a/(100000*v^x) 
  return(a)
}

aaik <- function(x,n,t){
  a <- 0
  lp <- 100000
  for (i in 0:(n-1)) {
    #print(v^(x+i+0.5)  * lp %*% i_xy[[x+i,2]] *  (1-(0.5*q_xya[[x+i,2]])))
    a <- a + v^(x+i+0.5)  * lp %*% i_xy[[x+i,4]] *  (1-(0.5*q_xya[[x+i,4]])) *  max(0.5 * (aik(x+i,x+i,t-i)+aik(x+i+1,x+i+1,t-i-1))-0.5, 0)
    lp <- lp * (1-q_xya[[x+i,4]]- i_xy[[x+i,4]])
  }
  a <- a/(100000*v^x) 
  return(a)
}

#Funktionen zur Prämienberechnung
#Markov--------------------------------------------------
aa <- function(x,t){
  a <- 1
  e <- c(1,rep(0,7))
  if(t == 1){
    return(a)
  } else{
    for(i in 1:(t-1)){
      a <- a + e %*% pm(x,i) %*%  e * v^i
    }
    return(a)
  }
} 

ai <- function(z,x,l){
  a <- 1
  e <- rep(0,8)
  if(l == 0){
    return(0)
  } 
  if(l == 1){
    return(a)
  } 
  if(x-z < 5){
    e[x-z+2] <- 1 
  } else{
    e[7] <- 1
  }
  for(i in 0:(l-1)){
    a <- a + e %*% pm(x,i) %*%  c(0,rep(1,6),0) * v^i
    }
  return(a)
}

aai <- function(x,n,l){
  a <- 0
  if(l==0){
    return(0)
  }
  if(n==0){
    return(0)
  }
  for (i in 0:(n-1)){
      #print(max(0.5 * (ai(x+i,x+i,t-i)+ai(x+i+1,x+i+1,t-i-1) - 1), 0))
      #print(v^(x+i+0.5)  %*% pm(x,i)[1,1] %*% i_xy[[x+i,2]] *  (1-(0.5*q_xya[[x+i,2]])))
    a <- a + v^(i+0.5)  %*% pm(x,i)[1,1] %*% i_xy[[x+i,4]] *  (1-(0.5*q_xya[[x+i,4]])) *  max((0.5 * (ai(x+i,x+i,l-i)+ai(x+i+1,x+i+1,l-i-1))-0.5), 0)
  }
  return(a)
}

#Studie der Barwerte etc.
# verlauf barwerte versch. Eintrittsalter
aailvec <- NULL
for (i in 1:55) {
  aailvec <- c(aailvec,aai(1,i,i))
}
alvec <- rbind(1:length(aailvec),aailvec)
aldata <- data.frame(t(alvec))
aldata2[,2] <- aldata2[,2] / 1.11
aldata2[,1] <- aldata2[,1] +14

aailvec2 <- NULL
for (i in 1:54) {
  aailvec2 <- c(aailvec2,aai(i,2,2))
}
alvec2 <- rbind(1:length(aailvec2),aailvec2)
aldata2 <- data.frame(t(alvec2))
aldata2[52,2] <- aldata2[52,2] - 0.0075
aldata2[,2] <- aldata2[,2] + 14

ggal <- NULL
ggal <- ggplot() + 
  geom_point(data = aldata, aes(V1,aailvec,col = "Anwartschaft")) +
  scale_colour_manual("Legende:",breaks = c("Anwartschaft"),values = c("black")) +
  xlab("Versicherungsdauer") +
  ylab("Barwert") +
  scale_x_continuous(breaks = seq(0,60,by=10))+
  scale_y_continuous(breaks = seq(0,5, by=.5)) 

ggal2 <- NULL
ggal2 <- ggplot() + 
  geom_point(data = aldata2, aes(V1,aailvec2,col = "Anwartschaft")) +
  scale_colour_manual("Legende:",breaks = c("Anwartschaft"),values = c("black")) +
  xlab("Eintrittsalter") +
  ylab("Barwert") +
  scale_x_continuous(breaks = seq(0,70,by=10))+
  scale_y_continuous(breaks = seq(0,0.15, by=.01)) 


#verlauf Leistungsbarwert
aivec <- NULL
for (i in 1:55) {
  aivec <- c(aivec,ai(i,i,57-i))
}


aavec <- NULL
for (i in 1:55) {
  aavec <- c(aavec, aa(i,57-i))
}
avec <- rbind(15:(length(aavec)+14),aavec, aivec)
adata <- data.frame(t(avec))

ggrp <- ggplot() + geom_point(data = airpdata,col = "black", aes(V1,aivec))

#Plot Barwert Anwartschaft vs.Leistungsphase
gga <- NULL
gga <- ggplot() + 
  geom_point(data = adata, aes(V1,aavec,col = "Anwartschaft")) + 
  geom_point(data = adata, aes(V1,aivec,col = "Leistungsphase")) + 
  scale_colour_manual("Legende:",breaks = c("Anwartschaft","Leistungsphase"),values = c("black","red")) +
  xlab("Alter") +
  ylab("Barwert") +
  ggtitle("Barwert Anwartschaft/Leistungsphase") +
  scale_x_continuous(breaks = seq(0,70,by=10))+
  scale_y_continuous(breaks = seq(0,30, by=5)) 

#Nettoeinmalprämie Merkov(aaivec) vs. Nicht-Markov(aaikvec)
aaivec <- NULL
for (i in 1:55) {
  aaivec <- c(aaivec,aai(i,56-i,56-i))
}

aaikvec <- NULL
for (i in 1:55) {
  aaikvec <- c(aaikvec,aaik(i,56-i,56-i))
}

aaiavec <- rbind(15:(length(aaivec)+14),aaivec, aaikvec)
aaidata <- data.frame(t(aaiavec))

#Plot Barwert aai vs. aaik (Nettoeinmalprämie Markov/Nicht-Markov)
ggnepp <- ggplot() + 
  geom_point(data = aaidata, aes(V1,aaivec, colour = "NEP-M(1997)")) + 
  geom_point(data = aaidata, aes(V1,aaikvec, colour = "NEP-NM(1997)")) + 
  scale_colour_manual("",breaks = c("NEP-M(1997)","NEP-NM(1997)"),values = c("black","red")) +
  xlab("Eintrittsalter") +
  ylab("Barwert") +
  scale_x_continuous(breaks = seq(0,80,by=10))+
  scale_y_continuous(breaks = seq(0,4.5, by=0.5))

#Prämienbarwert zur Berechnung der Netto(jahres)prämie Markov(NP) vs.  Nicht-Markov(NPk)
aavec <- NULL 
for (i in 1:55) {
  aavec <- cbind(aavec,rbind(1,aa(i,56-i), aak(i,56-i)))
}

#Nettojahresprämie
nettop <- aaiavec / aavec
nettopdata <- data.frame(t(nettop))

#Plot 
#Nettojahresprämie Markov/Nicht-Markov
ggnettopp <- ggplot() + 
  geom_point(data = nettopdata, aes(V1,aaivec, colour = "NP-M(1997)")) + 
  geom_point(data = nettopdata, aes(V1,aaikvec, colour = "NP-NM(1997)")) + 
  scale_colour_manual("",breaks = c("NP-M(1997)","NP-NM(1997)"),values = c("black","red")) +
  xlab("Eintrittsalter") +
  ylab("Barwert") +
  scale_x_continuous(breaks = seq(0,80,by=10))+
  scale_y_continuous(breaks = seq(0,0.3, by=0.05))


diffbonus <- NULL
diffbonus <- (rbind(15:(length(nettop[1,])+14),((nettop[2,]/nettop[3,])-1)*100, ((aaiavec[2,]/aaiavec[3,])-1)*100))
diffbonus[is.nan(diffbonus)] <- 0
diffbondata <- data.frame(t(diffbonus))

#Plot 
ggdiffbon <- ggplot() + 
  geom_point(data = diffbondata, aes(X1,X2, colour = "NPdiff")) + 
  geom_point(data = diffbondata, aes(X1,X3, colour = "NEPdiff")) + 
  scale_colour_manual("",breaks = c("NPdiff","NEPdiff"),values = c("black","red")) +
  xlab("Alter") +
  ylab("diff(%)") +
  ggtitle("Prämien-Differenz Markov/Nicht-Markov") +
  scale_x_continuous(breaks = seq(0,80,by=10))+
  scale_y_continuous(breaks = seq(0,50, by=10))

#verlauf nettodeckungskapital
vmxfunc <- function(x,n,l,m){
    vmx <- aai(x+m,n-m,l-m) - ((aai(x,n,l)/aa(x,n)) *aa(x+m,n-m))
    return(c(m,vmx))
}

vma <- c(0,0)
for (i in 1:24) {
  vma <- rbind(vma,vmxfunc(25,25,25,i))
  print(i)
}


vmadata <- data.frame(vma)
vmadata[,2] <- vmadata[,2]*0.8
ggvma <- NULL
ggvma <- ggplot() + geom_point(data = vmadata, aes(X1,X2, colour ="Anwartschaft"), size=2.5)+labs(y = expression(mVx), x = "m") + scale_colour_manual("",
                                                                                                         breaks = c("Anwartschaft"),
                                                                                                         values = c("black"))

vmi <- NULL
for (i in 15:24){
  vmi <- rbind(vmi, c(i,ai(40,25+i,25-i)))
}

vmidata <- data.frame(vmi)


ggvmai <- NULL
ggvmai <- ggplot() + geom_point(data = vmadata, aes(X1,X2, colour ="Anwartschaft"), size=2.5)+labs(y = expression(mVx), x = "m")
ggvm <- ggvmai + geom_point(data = vmidata, aes(X1,X2, colour ="Leistungsphase"), size=2.5) +
scale_colour_manual("", 
                    breaks = c("Anwartschaft", "Leistungsphase"),
                    values = c("black", "red"))
ggtest <- ggplot(NULL,aes(x,y)) + geom_point(vmidata)

#plot
q <- NULL
for (j in 1:55){
  q <- c(q,axxl(j,56-j))
}
qdata <- data.frame(cbind(q, 1:length(q)))
ggl <- ggplot() + geom_line(data = qdata, col = "black", aes(V2, q))
#
#
#aai <- function(x,l){
#  t <- 1
#  for(j in 1:l-1){
#    print(axxl(x+j,l-j))
#    print(axxl(x+j+1,l-j-1))
#    print(x+j)
#    print(l-j)
#    t <- t + pm(x,j)[1,1] * (1/(1+0.02))^(j+0.5) * i_xy[[x+j,2]] * (1-(0.5*q_xya[[x+j,2]])) * max(0.5 * (axxl(x+j,l-j)+axxl(x+j+1,l-j-1))-1, 0)
#  }
#  return(t)
#}


