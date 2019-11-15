// Purpose: Connect to Hana and execute a query
// Author : Simon Li
// Date   : Nov 11, 2019
//
// Usage : 
// const hanaquery = require('hanaquery');
// const hana = new hanaquery();

"use strict";

const hanaClient = require("@sap/hana-client");
const connection = hanaClient.createConnection();

class myhdb {
    constructor(host, port, user, passwd, dbo) {
        this.connectionParams = {
            host: host,
            port: port,
            uid: user,
            pwd: passwd,
            databaseName: dbo 
        }
    }

    exec(sql, callback) {
        connection.connect(this.connectionParams, err => {
            if (err) throw new Error("Connection error" + err);
        
            connection.exec(sql, (err, rows) => {
                connection.disconnect();
        
                if (err) throw new Error('SQL execute error:' + err);
        
                //console.log("Results:", rows);
                //console.log(`Query '${sql}' returned ${rows.length} items`);
                // rows is an array of objects
                if (typeof(callback) != 'undefined')
                    callback(rows);
            });
        });        
    }
    
    async execA(sql) {
        await connection.connect(this.connectionParams);
       
        const rows = await connection.exec(sql);
        connection.disconnect();
        return rows;
    }
}
module.exports = myhdb;

if (require.main === module) {
    //console.log('called directly');
    const hana = new myhdb("hxehost", 39013, "SYSTEM", "********", "HXE");

    const whereClause = process.argv[2] ? `WHERE "group" = '${process.argv[2]}'` : "";
    const sql         = `SELECT "name" FROM food_collection ${whereClause}`;
    hana.exec(sql, rows => {
        console.log("Results:", rows);
    })
    // promisified
    hana.execA(sql).then(rows => console.log("Results:", rows));
}

