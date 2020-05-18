module.exports = {
    plugins: [
        {
            resolve: `gatsby-source-filesystem`,
            options: {
                name: `src`,
                path: `{__dirname}/`
            }
        },
        'gatsby-transformer-remark'
    ]
}
