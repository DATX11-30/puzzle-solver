"use server";
import { exec } from "child_process";
import { cwd } from "process";

export async function getSolutionFromFile(fileName: string): Promise<string> {
	return new Promise((resolve, reject) => {
		exec("./out " + "./sudokus/" + fileName + ".txt", { cwd: cwd() }, (error, stdout, stderr) => {
			if (error) {
				reject(error);
			}
			if (stderr) {
				reject(stderr);
			}
			resolve(stdout);
		});
	});
}

export async function getAllSudokus(): Promise<string> {
	throw new Error("Not implemented");
}

export async function getSolution(
	fileName: string
): Promise<{ partialSuds: string[]; techniques: string[] }> {
	const solution = await getSolutionFromFile(fileName);
	const solutionArray = solution.split("\\n");

	solutionArray.pop();

	const partialSuds = solutionArray.splice(1);

	const techniques = ["Not implemented"];

	return { partialSuds, techniques };
}
