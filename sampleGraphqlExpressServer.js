// Graphql Express Server Demo - Message 
//
// Purpose: Sample to use graphql express server to expose APIs
//          supports APIs for (C)reate, (R)ead, (U)pdate/Patch, 
//          (D)elete and Query
// Author : Simon Li  Jan 19, 2020
//
// Prerequisite packages: 
//        dotenv, sequelize, mysql2, express, express-graphql, graphql
// 
// No source code change and direct support with the backend
// ORM for Postgres, MySQL, MariaDB, SQLite and Microsoft SQL Server.
//
// Tested with GraphiQL, postman GraphQL and curl
'use strict';

//-----------------------------------------
// Part 1 - sequelize - database table sync
//-----------------------------------------
// Load the enviroment variables to process from .env
require('dotenv').config()
const databaseUrl = process.env.DATABASEURL;

const Sequelize = require('sequelize');
const {DataTypes} = require('sequelize');

// src/data/sequelize.js
const sequelize = new Sequelize(databaseUrl, {
    define: {
      freezeTableName: true
    },
    logging: console.log
});

// src/data/models/messageTable.js
const dataModel = sequelize.define('messages', {
    id: {
           type: DataTypes.INTEGER,
           allowNull: false,
           primaryKey: true,
           autoIncrement: true
    }, 
	content: {
        type: DataTypes.STRING,
        allowNull: false,
	},
	author: {
        type: DataTypes.STRING,
	}
});

// src/server.js
dataModel.sync().then(() => { 
    console.log("Database connected!!")
    }).catch(error => {
    throw new Error("oooh, did you enter wrong database credentials?");
});

//---------------------------
// Part 2 - graphql
//---------------------------
const express = require('express');
const graphqlHTTP = require('express-graphql');
const graphql = require('graphql');

//construct a schema programmatically.
// src/data/types/CountryType.js
const messageType = new graphql.GraphQLObjectType({
    name: 'Message',
    fields: {
                id: { type: graphql.GraphQLInt },
                content: { type: graphql.GraphQLString },
                author: { type: graphql.GraphQLString }
    }
});

// Define the Query type
// src/data/queries/getCountries.js
const getMessage = {
    type: messageType,
    args: {
        id: { type: graphql.GraphQLInt }
    },
    async resolve(_, {id}) {
        return await dataModel.findOne({where: {id: id}});
    }
};

const getMessageList = {
    type: new graphql.GraphQLList(messageType),
    async resolve() {
        return await dataModel.findAll();
    }
};

// src/data/mutations/updateMessage.js
const createMessage = {
    type: messageType,
    args: {
        content: { type: graphql.GraphQLString },
        author: { type: graphql.GraphQLString }
    },
    async resolve(_, {content, author}) {
      const message = {content, author};
      console.log(message);
      return await dataModel.create({content: content, author: author});
  }
};

const updateMessage = {
    type: graphql.GraphQLString,
    args: { 
        id: { type: graphql.GraphQLInt },
        content: { type: graphql.GraphQLString },
        author: { type: graphql.GraphQLString }
    },
    async resolve(_, {id, content, author}) {
        const message = {};
        if (content) message["content"] = content;
        if (author) message["author"] = author;

        const result = await dataModel.update(message, {where: {id: id}});
        return `${result} record(s) patched - ${id}`;
    }
};

const deleteMessage = {
    type: graphql.GraphQLString,
    args: {
        id: { type: graphql.GraphQLInt }
    },
    async resolve(_, {id}) {
        const result = await dataModel.destroy({where: {id: id}});
        return `Number of records deleted: ${result}`;
    }
};

// src/data/schema.js
const schema = new graphql.GraphQLSchema({
    query: new graphql.GraphQLObjectType({
        name: 'Query',
        fields: {
            getMessage,
            getMessageList
        }
    }),
    mutation: new graphql.GraphQLObjectType({
        name: 'Mutation',
        fields: {
            createMessage,
            updateMessage,
            deleteMessage
        }
    })
});

//---------------------------
// Part 3 - server.js
//---------------------------
// src/server.js
const app = express();
// Dummy root request
app.get("/", (req, res) => {
    res.send("<h4>Welcome to message services powered by express-graphql programming & sequelize/mysql.</h4>\n");
})

app.use('/graphql', graphqlHTTP({
    schema: schema,
    graphiql: true,  // GrapgiQL UI will be launched
    pretty: true
}));

const port = 4000;
app.listen(port);
console.log(`Running a GraphQL API server at http://localhost:${port}/graphql`);

//-------------------------------
// Part 4 - GraphiQL test samples
//-------------------------------
/* 
mutation {
  createMessage(author: "andy", content: "hope is a good thing") {
    id author content
  }
}
=======================================================================
mutation {
  updateMessage(id: 3, author: "andy", content: "andy's test")
}
======================================================================
{
  getMessage(id: 1) {
    id content author
  }
}
======================================================================
mutation {
  deleteMessage(id: 2)
}
======================================================================
{
  getMessageList {
    id
    author
    content
  }
}
*/

//-------------------------------
// Part 5 - curl test samples
//-------------------------------
/*
curl -i -H 'Content-Type: application/json' -X POST -d '{"query": "query {getMessageList{id author content}}"}' http://localhost:4000/graphql
curl -i -H 'Content-Type: application/json' -X POST -d '{"query": "query {getMessage(id: 1) {id author content}}"}' http://localhost:4000/graphql
curl -i -H 'Content-Type: application/json' -X POST -d '{"query": "mutation {createMessage(author: \"curl\", content: \"test\") {id author content}}"}' http://localhost:4000/graphql
curl -i -H 'Content-Type: application/json' -X POST -d '{"query": "mutation {updateMessage(id: 9, content: \"curl test\")}"}' http://localhost:4000/graphql
curl -i -H 'Content-Type: application/json' -X POST -d '{"query": "mutation {deleteMessage(id: 9)}"}' http://localhost:4000/graphql
*/