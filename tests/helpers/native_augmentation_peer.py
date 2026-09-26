"""Python n4m oracle for the product R Gaussian training augmentation."""

import json

import numpy as np

from n4m.augmentation.noise import GaussianAdditiveNoise


samples = np.arange(1, 31)
features = np.arange(1, 9)
X = (np.sin(np.outer(samples, features) / 11)
     + np.cos((samples[:, None] + features[None, :]) / 7)
     + np.outer(samples, features) / 100)
augmented = GaussianAdditiveNoise(sigma=0.03, seed=42).transform(X)
print(json.dumps(augmented.tolist()))
