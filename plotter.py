import numpy as np
import matplotlib.pyplot as plt
import os

# Folder containing your CSV matrix files
INPUT_FOLDER = "matrices"
OUTPUT_FOLDER = "images"
COLORMAP = "plasma"  # Try: 'viridis', 'inferno', 'magma', 'cividis', etc.

os.makedirs(OUTPUT_FOLDER, exist_ok=True)

def process_matrix_file(file_path, output_path, cmap=COLORMAP):
    # Load matrix from CSV
    matrix = np.loadtxt(file_path, delimiter=",", dtype=int)

    # Normalize for display (optional)
    matrix_normalized = matrix / matrix.max() if matrix.max() != 0 else matrix

    # Plot the matrix
    plt.figure(figsize=(10, 6))
    plt.imshow(matrix_normalized, cmap=cmap, interpolation='nearest')
    plt.axis("off")  # Hide axes for artistic look
    plt.savefig(output_path, bbox_inches="tight", pad_inches=0)
    plt.close()

# Batch-process all CSV files
for filename in os.listdir(INPUT_FOLDER):
    if filename.endswith(".csv"):
        input_path = os.path.join(INPUT_FOLDER, filename)
        output_path = os.path.join(OUTPUT_FOLDER, filename.replace(".csv", ".png"))
        print(f"Processing {filename} -> {output_path}")
        process_matrix_file(input_path, output_path)

print("✅ All images generated.")

