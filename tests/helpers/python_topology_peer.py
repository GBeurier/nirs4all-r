"""Read an R-exported recipe with nirs4all Python's topology analyzer."""

import json
import sys


def main() -> None:
    recipe_path, result_path, python_repo = sys.argv[1:4]
    sys.path.insert(0, python_repo)
    from nirs4all.pipeline.analysis.topology import analyze_topology

    with open(recipe_path, encoding="utf-8") as stream:
        recipe = json.load(stream)
    topology = analyze_topology(recipe["pipeline"])
    result = {
        "feature_merge": topology.has_feature_merge,
        "stacking": topology.has_stacking,
        "models": len(topology.model_nodes),
        "branches_without_merge": topology.has_branches_without_merge,
    }
    with open(result_path, "w", encoding="utf-8") as stream:
        json.dump(result, stream)


if __name__ == "__main__":
    main()
