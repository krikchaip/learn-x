import {MigrationInterface, QueryRunner} from "typeorm";

export class AddUsernameIndex1612635035274 implements MigrationInterface {
    name = 'AddUsernameIndex1612635035274'

    public async up(queryRunner: QueryRunner): Promise<void> {
        await queryRunner.query(`CREATE INDEX "IDX_b67337b7f8aa8406e936c2ff75" ON "public"."user" ("username") `);
    }

    public async down(queryRunner: QueryRunner): Promise<void> {
        await queryRunner.query(`DROP INDEX "public"."IDX_b67337b7f8aa8406e936c2ff75"`);
    }

}
