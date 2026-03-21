from pathlib import Path
import json
import numpy as np

from sklearn.feature_extraction.text import TfidfVectorizer
from sklearn.linear_model import LogisticRegression
from sklearn.metrics import confusion_matrix, classification_report, roc_auc_score

DATA_DIR = Path("data")

with open(DATA_DIR / "train_core_vs_neg.json", "r", encoding="utf-8") as f:
    train_data = json.load(f)

with open(DATA_DIR / "test_core_vs_neg.json", "r", encoding="utf-8") as f:
    test_data = json.load(f)

X_train_texts = [t for (t, y) in train_data]
y_train = [y for (t, y) in train_data]

X_test_texts = [t for (t, y) in test_data]
y_test = [y for (t, y) in test_data]

print(f"Train size: {len(X_train_texts)}")
print(f"Test size:  {len(X_test_texts)}")

vectorizer = TfidfVectorizer(
    lowercase=True,
    min_df=5,
    max_df=0.9
)

X_train = vectorizer.fit_transform(X_train_texts)
X_test = vectorizer.transform(X_test_texts)

print(f"\nTF-IDF matrix shapes:")
print(f"  Train: {X_train.shape}")
print(f"  Test:  {X_test.shape}")

clf = LogisticRegression(penalty="l1", solver="liblinear", max_iter=2000)
clf.fit(X_train, y_train)

y_pred = clf.predict(X_test)
y_prob = clf.predict_proba(X_test)[:, 1]

print("\n" + "=" * 60)
print("L1 MODEL EVALUATION")
print("=" * 60)

cm = confusion_matrix(y_test, y_pred)
print("\nConfusion matrix:")
print(cm)

print("\nClassification report:")
print(classification_report(y_test, y_pred))

auc = roc_auc_score(y_test, y_prob)
print(f"ROC AUC: {round(auc, 3)}")

coefs = clf.coef_[0]
n_nonzero = np.sum(coefs != 0)
print(f"\nSparsity diagnostic:")
print(f"  Total coefficients:    {len(coefs)}")
print(f"  Non-zero coefficients: {n_nonzero}")
print(f"  Zero coefficients:     {len(coefs) - n_nonzero}")

feature_names = vectorizer.get_feature_names_out()
sorted_indices = np.argsort(coefs)

print("\n" + "-" * 60)
print("Top 15 POSITIVE-weight words (most predictive of CORE = 1):")
print("-" * 60)
for i in sorted_indices[-15:][::-1]:
    print(f"  {feature_names[i]:25s}  {coefs[i]:.4f}")

print("\n" + "-" * 60)
print("Top 15 NEGATIVE-weight words (most predictive of NEG = 0):")
print("-" * 60)
for i in sorted_indices[:15]:
    print(f"  {feature_names[i]:25s}  {coefs[i]:.4f}")
