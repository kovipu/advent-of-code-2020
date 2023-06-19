'use strict';
import fs from 'fs';
import request from 'request';
import path from 'path';
import { exec } from 'child_process';
import dotenv from 'dotenv';
dotenv.config();


const bootstrap = (year, day) => {
    // Left-pad day!
    day = `${day}`;
    if (day.length == 1) {
        day = `0${day}`;
    }
    // Write file
    fs.mkdir(`src/Year${year}`, { recursive: true }, function(err) {
        if (err) {
            console.log(err);
            return;
        }
        fs.readFile('templates/DayXX.purs', 'utf8', function(err, body) {
            let b = body.replace(/__YEAR__/g, year).replace(/__DAY__/g, day);
            fs.writeFile(`src/Year${year}/Day${day}.purs`, b, function(err) {
                if (err) {
                    console.log(err);
                    return;
                }
                console.log(`Open src/Year${year}/Day${day}.purs in your editor`);
            });
        });
    });
};

const download = (year, day, cookie, filepath, cb) => {
    const exists = fs.existsSync(filepath);
    if (exists) {
        console.log(`${filepath} already downloaded`);
        if (cb) cb();
    } else {
        request(`https://adventofcode.com/${year}/day/${day}/input`, {
            headers: {
                'Cookie': `session=${cookie}`
            }
        }, function(err, res, body) {
            if (err) {
                console.log(err);
                return;
            }
            fs.mkdir(path.dirname(filepath), { recursive: true }, function(err) {
                if (err) {
                    console.log(err);
                    return;
                }
                fs.writeFile(filepath, body, function(err) {
                    if (err) {
                        console.log(err);
                        return;
                    }
                    if (cb) cb();
                    else console.log(`Downloaded ${filepath}`);
                });
            });
        });
    }
};

const run = (year, day, part, inputfilepath) => {
    // Left-pad day!
    day = `${day}`;
    if (day.length == 1) {
        day = `0${day}`;
    }
    exec('npx spago build', function(err) {
        if (err) {
            console.log(err);
            return;
        }
        fs.readFile(inputfilepath, 'utf8', async (err, body) => {
            if (err) {
                console.log(err);
                return;
            }
            const module = await import(`../../output/Year${year}.Day${day}/index.js`);
            if (part == 1 || !part) {
                console.time("Obtained in");
                module.part1(body)();
                console.timeEnd("Obtained in");
            }
            if (part == 2 || !part) {
                console.time("Obtained in");
                module.part2(body)();
                console.timeEnd("Obtained in");
            }
        });
    });
};

const aoc_cookie = () => {
    return process.env.AOC_COOKIE;
};

export { bootstrap, download, run, aoc_cookie }
