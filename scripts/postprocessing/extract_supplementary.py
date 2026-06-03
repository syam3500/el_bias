import os
import re
import zipfile
import shutil

# Paths derived from this script's location so the repo is portable.
HERE = os.path.dirname(os.path.abspath(__file__))
REPO_ROOT = os.path.normpath(os.path.join(HERE, "..", ".."))

# Root directory for results (per-AET .md outputs live in results/scenarios/AET_*)
results_root = os.path.join(REPO_ROOT, "results", "scenarios")
output_tex = os.path.join(REPO_ROOT, "results", "supplementary_plots.tex")
extracted_figures_dir = os.path.join(REPO_ROOT, "results", "figures_extracted")

if not os.path.exists(extracted_figures_dir):
    os.makedirs(extracted_figures_dir)


def aet_from_folder(md_path):
    """Extract the AET value (µg/mL) from the parent folder name, e.g. 'AET 0.45' -> '0.45'."""
    parent = os.path.basename(os.path.dirname(md_path))  # e.g. 'AET_0.45'
    m = re.match(r"AET[\s_]+([\d.]+)", parent, re.IGNORECASE)
    return m.group(1) if m else "?"


def parse_filename(fname, aet_val):
    """Return a descriptive section title built from the filename and the AET folder value.

    Filename structure (after stripping .md):
      GC_MS_<surr>_<ConcID><RRF_ID>[_<suffix>]
      LC_MS+_<surr>_<ConcID><RRF_ID>[_<suffix>]
      LC_MS-_<surr>_<ConcID><RRF_ID>[_<suffix>]
      LC_MS_X_<ConcID><RRF_ID>[_<suffix>]   (no-ion variant, surrogate = X)

    <surr>   : surrogate concentration (5, 10, 20 µg/mL, or X = unknown/mixed)
    <ConcID> : 1 = Uniform, 2 = Log-Normal, 3 = Custom Mixture
    <RRF_ID> : A = Empirical Sampling, B = Simulated Normal
    <suffix> : 0.60 / 0.70 (uniform fraction variant) or GC / LC (fitted log-normal)
    """
    base = fname.replace(".md", "")

    if "GC_MS" in base:
        tech = "GC/MS"
        parts_rem = base.replace("GC_MS_", "").split("_")
    elif "LC_MS+" in base:
        tech = "LC/MS+"
        parts_rem = base.replace("LC_MS+_", "").split("_")
    elif "LC_MS-" in base:
        tech = "LC/MS-"
        parts_rem = base.replace("LC_MS-_", "").split("_")
    elif "LC_MS" in base:
        tech = "LC/MS"
        parts_rem = base.replace("LC_MS_", "").split("_")
    else:
        parts = base.split("_")
        tech = parts[0]
        parts_rem = parts[1:]

    # parts_rem: ['10', '1A', '0.60'] or ['X', '2B', 'LC'] etc.
    surr     = parts_rem[0] if len(parts_rem) > 0 else "?"
    id_code  = parts_rem[1] if len(parts_rem) > 1 else "?"
    suffix   = parts_rem[2] if len(parts_rem) > 2 else ""

    # Surrogate concentration label
    surr_label = f"{surr} µg/mL" if surr != "X" else "X (unknown)"

    # Concentration distribution description
    conc_id = id_code[0] if len(id_code) > 0 else ""
    if conc_id == '1':
        if suffix == "0.60":
            conc_desc = "Uniform (40% >= AET)"
        elif suffix == "0.70":
            conc_desc = "Uniform (30% >= AET)"
        else:
            conc_desc = "Uniform (66.7% >= AET)"
    elif conc_id == '2':
        conc_desc = "Log-Normal (Fitted)" if suffix in ["GC", "LC"] else "Log-Normal (Standard)"
    elif conc_id == '3':
        conc_desc = "Custom Mixture"
    else:
        conc_desc = "?"

    # RRF description
    rrf_id = id_code[1] if len(id_code) > 1 else ""
    rrf_desc = {"A": "Empirical Sampling", "B": "Simulated Normal"}.get(rrf_id, "?")

    # AET from folder, surrogate from filename
    title = (f"{tech} | AET {aet_val} µg/mL | "
             f"Surrogate {surr_label} | {conc_desc} | {rrf_desc}")
    return title


def find_images_in_md(md_path):
    with open(md_path, 'r', encoding='utf-8', errors='ignore') as f:
        content = f.read()

    # Find the Trade-off plot image (first match is fine — there is only one)
    tradeoff_img = None
    match = re.search(
        r"Trade-off: False Positives vs\. False Negatives.*?!\[\]\((.*?)\)",
        content, re.DOTALL)
    if match:
        tradeoff_img = match.group(1).strip()

    # Find the LAST "Average Count per Simulation WITH ± SD" image (raw counts,
    # NOT the percentage version).  GC_MS_X files only contain the % variant
    # and will correctly return None here, causing them to be skipped.
    conf_img = None
    all_conf_matches = re.findall(
        r"Average Count per Simulation WITH\s*\\u00b1\s*SD.*?!\[\]\((.*?)\)",
        content, re.DOTALL)
    if all_conf_matches:
        conf_img = all_conf_matches[-1].strip()

    return tradeoff_img, conf_img


# ── LaTeX preamble ──────────────────────────────────────────────────────────
tex_content = [
    r"\documentclass{article}",
    r"\usepackage[utf8]{inputenc}",
    r"\usepackage{graphicx}",
    r"\usepackage{subcaption}",
    r"\usepackage[margin=1in]{geometry}",
    r"\usepackage{float}",
    r"\title{Supplementary Simulation Results}",
    r"\author{Master Thesis}",
    r"\date{\today}",
    r"\begin{document}",
    r"\maketitle",
    r"\tableofcontents",
    r"\newpage"
]

# ── Collect all .md files ───────────────────────────────────────────────────
all_mds = []
for root, dirs, files in os.walk(results_root):
    # Skip hidden / cache directories
    dirs[:] = [d for d in dirs if not d.startswith('.')]
    for file in files:
        if file.endswith(".md"):
            all_mds.append(os.path.join(root, file))

# Sort: primarily by AET folder (numeric), then by filename
def sort_key(p):
    folder = os.path.basename(os.path.dirname(p))
    m = re.match(r"AET[\s_]+([\d.]+)", folder, re.IGNORECASE)
    aet_num = float(m.group(1)) if m else 0
    return (aet_num, os.path.basename(p))

all_mds.sort(key=sort_key)

# ── Process each file ───────────────────────────────────────────────────────
for md_path in all_mds:
    fname = os.path.basename(md_path)
    if len(fname) < 10 or "combined" in fname:
        continue

    aet_val      = aet_from_folder(md_path)
    section_title = parse_filename(fname, aet_val)
    print(f"Processing [{aet_val} µg/mL] {fname} ...")

    tradeoff_rel, conf_rel = find_images_in_md(md_path)

    if not tradeoff_rel or not conf_rel:
        print(f"  !! Missing images — skipping")
        continue

    md_dir       = os.path.dirname(md_path)
    tradeoff_abs = os.path.join(md_dir, tradeoff_rel.replace("/", os.sep))
    conf_abs     = os.path.join(md_dir, conf_rel.replace("/", os.sep))

    # Unzip if images are packed
    files_dir_name = tradeoff_rel.split("/")[0]
    zip_path = os.path.join(md_dir, files_dir_name + ".zip")
    if not os.path.exists(tradeoff_abs) and os.path.exists(zip_path):
        print(f"  Unzipping {files_dir_name}.zip ...")
        with zipfile.ZipFile(zip_path, 'r') as zf:
            zf.extractall(md_dir)

    # Build unique output filenames that include the AET value so files from
    # different AET folders never overwrite each other.
    aet_tag   = aet_val.replace(".", "_")          # e.g. "0_45" or "6"
    safe_name = fname.replace(".", "_")             # e.g. "GC_MS_10_1A_0_60_md"
    tradeoff_dest = os.path.join(
        extracted_figures_dir, f"AET{aet_tag}_{safe_name}_tradeoff.png")
    conf_dest = os.path.join(
        extracted_figures_dir, f"AET{aet_tag}_{safe_name}_conf.png")

    try:
        shutil.copy(tradeoff_abs, tradeoff_dest)
        shutil.copy(conf_abs,     conf_dest)
    except Exception as e:
        print(f"  !! Error copying images: {e}")
        continue

    # ── LaTeX section ───────────────────────────────────────────────────────
    # Escape characters that are special in LaTeX
    tex_title = (section_title
                 .replace("_", r"\_")
                 .replace("%", r"\%")
                 .replace("µ", r"$\mu$"))

    tex_content.append(r"\section{" + tex_title + "}")
    tex_content.append(r"\begin{figure}[H]")
    tex_content.append(r"  \centering")
    tex_content.append(r"  \begin{subfigure}[b]{0.48\textwidth}")
    tex_content.append(
        r"    \includegraphics[width=\textwidth]{"
        + tradeoff_dest.replace("\\", "/") + "}")
    tex_content.append(r"    \caption{Trade-off Plot}")
    tex_content.append(r"  \end{subfigure}")
    tex_content.append(r"  \hfill")
    tex_content.append(r"  \begin{subfigure}[b]{0.48\textwidth}")
    tex_content.append(
        r"    \includegraphics[width=\textwidth]{"
        + conf_dest.replace("\\", "/") + "}")
    tex_content.append(r"    \caption{Average Count per Simulation WITH $\\pm$ SD}")
    tex_content.append(r"  \end{subfigure}")
    tex_content.append(r"  \caption{" + tex_title + "}")
    tex_content.append(r"\end{figure}")
    tex_content.append(r"\newpage")

tex_content.append(r"\end{document}")

with open(output_tex, "w", encoding='utf-8') as f:
    f.write("\n".join(tex_content))

print(f"\nFinished! LaTeX file written to:\n  {output_tex}")
