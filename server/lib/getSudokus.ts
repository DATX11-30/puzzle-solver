"use server";
import { readdir } from "fs/promises";

export async function getSudokus(difficulty: string) {
	const path = "./sudokus/";

	const files = await readdir(path);

	let sudokus: { filename: string; date: string; hasSolution: boolean }[] = [];
	files.map((filename) => {
		if (filename.includes("_sol")) return;

		const hasSolution = files.includes(filename.replace(".txt", "_sol.txt"));

		const date = filename.split(".")[0].split("_").splice(1).join("_");
		if (filename.includes(difficulty)) {
			sudokus.push({ filename, date, hasSolution });
		}
	});

	return sudokus;
}
