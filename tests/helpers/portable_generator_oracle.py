"""Independent nirs4all Python generator expansion oracle for the R tests."""

import json
import sys


def main() -> None:
    recipe_path, result_path, python_repo = sys.argv[1:4]
    sys.path.insert(0, python_repo)
    from nirs4all.pipeline.config._generator.core import expand_spec

    with open(recipe_path, encoding="utf-8") as stream:
        recipe = json.load(stream)
    result = []
    for variant in expand_spec(recipe["pipeline"]):
        steps = []
        for item in variant:
            steps.extend(item if isinstance(item, list) else [item])
        result.append({
            "classes": [step["class"] for step in steps[:-1]],
            "windows": [step.get("params", {}).get("window_length")
                        for step in steps[:-1]],
            "n_components": steps[-1]["model"]["params"]["n_components"],
        })
    with open(result_path, "w", encoding="utf-8") as stream:
        json.dump(result, stream)


if __name__ == "__main__":
    main()
