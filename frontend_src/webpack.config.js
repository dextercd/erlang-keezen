const path = require('path')

module.exports = {
    entry: './src/index.ts',
    module: {
        rules: [
            {
                test: /\.ts$/,
                use: 'ts-loader',
                exclude: '/node_modules/',
            },
        ],
    },
    resolve: {
        extensions: ['.ts', '.js'],
    },
    target: 'es3',
    output: {
        filename: 'bundle.js',
        path: path.resolve(__dirname, '../priv/static/script'),
    },
}
